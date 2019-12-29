/*
    This file is part of darktable,
    copyright (c) 2011 Henrik Andersson.

    darktable is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    darktable is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with darktable.  If not, see <http://www.gnu.org/licenses/>.
*/

#include "common/darktable.h"
#include "bauhaus/bauhaus.h"
#include "control/conf.h"
#include "gui/gtk.h"
#include "gui/accelerators.h"
#include "common/file_location.h"
#include <fcntl.h>

char midi_devices[] = "alsa,/dev/midi1,/dev/midi2,/dev/midi3,/dev/midi4";

DT_MODULE(1)

#ifdef HAVE_ALSA
  #define _GNU_SOURCE  /* the ALSA headers need this */
  #include <alsa/asoundlib.h>
#endif

#define D(stmnt) stmnt;

const char *name(dt_lib_module_t *self)
{
  return _("midi");
}

const char **views(dt_lib_module_t *self)
{
  static const char *v[] = {"darkroom", NULL};
  return v;
}

uint32_t container(dt_lib_module_t *self)
{
  return DT_UI_CONTAINER_PANEL_TOP_CENTER;
}

int expandable(dt_lib_module_t *self)
{
  return 0;
}

int position()
{
  return 1;
}

typedef struct dt_midi_knob_t
{
  gint group;
  gint channel;
  gint key;
  gint velocity;
  dt_accel_dynamic_t *accelerator;
  gboolean absolute;
  gboolean locked;
  gint pos_offset;
  gint neg_offset;
  gint neg_flip;
  float acceleration;
} dt_midi_knob_t;

typedef struct ControllerMidi
{
  gchar          *device;
  gint            midi_channel;
  gchar          *model_name;

  GIOChannel     *io;
  guint           io_id;

#ifdef HAVE_ALSA
  snd_seq_t      *sequencer;
  guint           seq_id;
  int             port;
#endif
  gboolean        name_queried;

  /* midi status */
  gboolean        swallow;
  gint            command;
  gint            channel;
  gint            key;
  gint            velocity;
  gint            msb;
  gint            lsb;

  gboolean        config_loaded;
  GSList         *mapping_list;

  gint            mapping_channel;
  gint            mapping_key;
  gint            mapping_velocity;

  gint            accum;
  gint            stored_channel;
  gint            stored_key;
  dt_midi_knob_t *stored_knob;

  gint            group;
  gint            group_switch_key;
} ControllerMidi;

static GtkWidget *mapping_widget = NULL;

typedef struct dt_lib_midi_t
{
  GSList *Controllers;
  gboolean SignalsConnected;
} dt_lib_midi_t;

void midi_save(ControllerMidi *midi)
{
  FILE *f = 0;

  gchar datadir[PATH_MAX] = { 0 };
  gchar midipath[PATH_MAX] = { 0 };

  dt_loc_get_user_config_dir(datadir, sizeof(datadir));
  snprintf(midipath, sizeof(midipath), "%s/midirc-%s", datadir, midi->model_name);

  f = g_fopen(midipath, "w");
  if(!f) return;

  g_fprintf(f,"group,channel,key,path,absolute,positive,negative,flip,accel\n");

  GSList *l = midi->mapping_list;
  while (l)
  {
    dt_midi_knob_t *k = (dt_midi_knob_t *)l->data;

    gchar *spath = g_strndup( k->accelerator->path,strlen(k->accelerator->path)-strlen("/dynamic") );

    g_fprintf(f,"%d,%d,%d,%s,%d,%d,%d,%d,%.4f\n", 
                k->group, k->channel, k->key, spath,
                k->absolute, k->pos_offset, k->neg_offset, k->neg_flip, k->acceleration);

    g_free(spath);

    l = g_slist_next(l);
  }  
  fclose(f);
}

void midi_load(ControllerMidi *midi)
{
  FILE *f = 0;

  int read = 0;
//  int defaults = 0;

  gchar datadir[PATH_MAX] = { 0 };
  gchar midipath[PATH_MAX] = { 0 };

  dt_loc_get_user_config_dir(datadir, sizeof(datadir));
  snprintf(midipath, sizeof(midipath), "%s/midirc-%s", datadir, midi->model_name);

  f = g_fopen(midipath, "rb");
  
  if(!f)
  {
    dt_loc_get_user_config_dir(datadir, sizeof(datadir));
    snprintf(midipath, sizeof(midipath), "%s/midirc_default-%s", datadir, midi->model_name);

    f = g_fopen(midipath, "rb");
    if(!f) return;
  }
  read = fscanf(f,"group,channel,key,path,absolute,positive,negative,flip,accel\n");
  while(!feof(f))
  {
    dt_midi_knob_t *k = (dt_midi_knob_t *)g_malloc(sizeof(dt_midi_knob_t));

    char accelpath[200];
    read = fscanf(f, "%d,%d,%d,%[^,],%d,%d,%d,%d,%f\n", 
                    &k->group, &k->channel, &k->key, accelpath, 
                    &k->absolute, &k->pos_offset, &k->neg_offset, &k->neg_flip, 
                    &k->acceleration);
    if(read > 0)
    {
      g_strlcat(accelpath,"/dynamic",200);
      GSList *al = darktable.control->dynamic_accelerator_list;
      dt_accel_dynamic_t *da;
      while(al)
      {
        da = (dt_accel_dynamic_t *)al->data;
        if (!g_strcmp0(da->path, accelpath))
          break;

        al = g_slist_next(al);
      }
      if (al)
      {
        k->accelerator = da;
        k->locked = FALSE;
        midi->mapping_list = g_slist_append(midi->mapping_list, k);
      }
      else
      {
        g_free(k);
      }
    }
  }
  fclose(f);
}

void refresh_knobs_to_controller(ControllerMidi *midi)
{
  GSList *l = midi->mapping_list;
  while(l)
  {
    dt_midi_knob_t *k = (dt_midi_knob_t *)l->data;
    GtkWidget *w = k->accelerator->widget;
    if (w)
    {
      float min = dt_bauhaus_slider_get_soft_min(w);
      float max = dt_bauhaus_slider_get_soft_max(w);
      float c   = dt_bauhaus_slider_get(w);
      
      int velocity = round((c-min)/(max-min)*127);

      if (midi->io)
      {
        gchar buf[3];
        buf[0] = (0xB << 4) + k->channel;
        buf[1] = k->key;
        buf[2] = velocity;

        int tmp = write(g_io_channel_unix_get_fd (midi->io), buf, 3);
        tmp++;
      }
#ifdef HAVE_ALSA
      else if (midi->sequencer)
      {
        snd_seq_event_t event;
        snd_seq_ev_clear(&event);

        event.type = SND_SEQ_EVENT_CONTROLLER;
        event.data.control.channel = k->channel;
        event.data.control.param = k->key;
        event.data.control.value = velocity;

        snd_seq_ev_set_subs(&event);  
        snd_seq_ev_set_direct(&event);
        snd_seq_ev_set_source(&event, midi->port);

        int alsa_error = snd_seq_event_output_direct(midi->sequencer, &event);
        if (alsa_error < 0)
        {
          D (g_print ("ALSA output error: %s\n", snd_strerror(alsa_error)));
        }
      }
#endif /* HAVE_ALSA */
    }
    l = g_slist_next(l);
  }
}

void refresh_knobs_to_controllers(GSList *l)
{
  while (l)
  {
    refresh_knobs_to_controller( (ControllerMidi *)l->data);
    l = g_slist_next(l);
  }
}

static gboolean knob_config_mode = FALSE;

static void slider_changed(GtkWidget *w, gpointer user_data)
{
  if (knob_config_mode)
  {
    mapping_widget = w;

    dt_control_hinter_message
            (darktable.control, _("configure midi controller knob\n"
                                  "   now carefully turn knob up one single notch"));
    
    knob_config_mode = FALSE;
  }
  else
  {
    refresh_knobs_to_controllers(((dt_lib_midi_t *) user_data)->Controllers);
  }

  return;
}

static void connect_slider_changed_callbacks(gpointer instance, dt_view_t *old_view,
                                             dt_view_t *new_view, gpointer data)
{
  dt_lib_midi_t *d = (dt_lib_midi_t *)data;

  if (new_view->view(new_view) == DT_VIEW_DARKROOM /*&& !d->SignalsConnected*/ )
  { 
    GSList *l = darktable.control->dynamic_accelerator_list;
    while(l)
    {
      dt_accel_dynamic_t *da = (dt_accel_dynamic_t *)l->data;

      g_signal_connect(G_OBJECT(da->widget), "value-changed", G_CALLBACK(slider_changed), data);

      l = g_slist_next(l);
    }  

    d->SignalsConnected = TRUE;
  }

  refresh_knobs_to_controllers( d->Controllers );
}

static void image_changed(gpointer instance, gpointer data)
{
  dt_lib_midi_t *d = (dt_lib_midi_t *)data;

  refresh_knobs_to_controllers( d->Controllers );
}

static gboolean      midi_read_event          (GIOChannel     *io,
                                               GIOCondition    cond,
                                               gpointer        data);


#ifdef HAVE_ALSA
static gboolean      midi_alsa_prepare        (GSource        *source,
                                               gint           *timeout);
static gboolean      midi_alsa_check          (GSource        *source);
static gboolean      midi_alsa_dispatch       (GSource        *source,
                                               GSourceFunc     callback,
                                               gpointer        user_data);

static GSourceFuncs alsa_source_funcs =
{
  midi_alsa_prepare,
  midi_alsa_check,
  midi_alsa_dispatch,
  NULL
};

typedef struct _GAlsaSource GAlsaSource;

struct _GAlsaSource
{
  GSource         source;
  ControllerMidi *controller;
};
#endif /* HAVE_ALSA */

static void controller_midi_init (ControllerMidi *midi)
{
  midi->device       = NULL;
  midi->midi_channel = -1;
  midi->model_name   = NULL;
  midi->io           = NULL;
  midi->io_id        = 0;
#ifdef HAVE_ALSA
  midi->sequencer    = NULL;
  midi->seq_id       = 0;
#endif
  midi->name_queried = FALSE;

  midi->swallow      = TRUE; /* get rid of data bytes at start of stream */
  midi->command      = 0x0;
  midi->channel      = 0x0;
  midi->key          = -1;
  midi->velocity     = -1;
  midi->msb          = -1;
  midi->lsb          = -1;

  midi->config_loaded    = FALSE;
  midi->mapping_list     = NULL;

  midi->mapping_channel  = -1;
  midi->mapping_key      = -1;
  midi->mapping_velocity = -1;

  midi->accum            =  0;
  midi->stored_channel   = -1;
  midi->stored_key       = -1;
  midi->stored_knob    = NULL;

  midi->group            =  0;
  midi->group_switch_key = -1;
}

gboolean midi_set_device(ControllerMidi *midi, const gchar *device)
{
  midi->swallow  = TRUE;
  midi->command  = 0x0;
  midi->channel  = 0x0;
  midi->key      = -1;
  midi->velocity = -1;
  midi->msb      = -1;
  midi->lsb      = -1;

  if (midi->io)
  {
    g_source_remove (midi->io_id);
    midi->io_id = 0;

    g_io_channel_unref (midi->io);
    midi->io = NULL;
  }

#ifdef HAVE_ALSA
  if (midi->seq_id)
  {
    g_source_remove (midi->seq_id);
    midi->seq_id = 0;

    snd_seq_close (midi->sequencer);
    midi->sequencer = NULL;
  }
#endif /* HAVE_ALSA */

  if (midi->device)
    g_free (midi->device);

  midi->device = g_strdup (device);

  if (midi->device && strlen (midi->device))
  {
    midi->model_name = "unknown";

    gint fd;

#ifdef HAVE_ALSA
    if (! g_ascii_strcasecmp (midi->device, "alsa"))
    {
      GSource *event_source;
      gint     ret;

      ret = snd_seq_open (&midi->sequencer, "default",
                          SND_SEQ_OPEN_DUPLEX, 0);
      if (ret >= 0)
      {
        snd_seq_set_client_name (midi->sequencer, _("darktable"));
        ret = snd_seq_create_simple_port (midi->sequencer,
                                          _("darktable MIDI"),
                                          SND_SEQ_PORT_CAP_WRITE |
                                          SND_SEQ_PORT_CAP_SUBS_WRITE |
                                          SND_SEQ_PORT_CAP_READ |
                                          SND_SEQ_PORT_CAP_SUBS_READ |
                                          SND_SEQ_PORT_CAP_DUPLEX,
                                          SND_SEQ_PORT_TYPE_APPLICATION);
        midi->port = ret;
      }

      if (ret < 0)
      {
        D (g_print(_("Device not available: %s"), snd_strerror (ret)));
        if (midi->sequencer)
        {
          snd_seq_close (midi->sequencer);
          midi->sequencer = NULL;
        }

        return FALSE;
      }

      event_source = g_source_new (&alsa_source_funcs,
                                   sizeof (GAlsaSource));

      ((GAlsaSource *) event_source)->controller = midi;

      midi->seq_id = g_source_attach (event_source, NULL);
      g_source_unref (event_source);

      return TRUE;
    }
#endif /* HAVE_ALSA */

#ifdef G_OS_WIN32
    fd = g_open (midi->device, O_RDWR, 0);
#else
    fd = g_open (midi->device, O_RDWR | O_NONBLOCK, 0);
#endif

    if (fd >= 0)
    {
      midi->io = g_io_channel_unix_new (fd);
      g_io_channel_set_close_on_unref (midi->io, TRUE);
      g_io_channel_set_encoding (midi->io, NULL, NULL);

      midi->io_id = g_io_add_watch (midi->io,
                                    G_IO_IN  | G_IO_ERR |
                                    G_IO_HUP | G_IO_NVAL,
                                    midi_read_event,
                                    midi);

      return TRUE;
    }
  }

  return FALSE;
}

/*
  Rotator relative encoding types
  +1,+2,+3  -1,-2,-3    pos_offset  neg_offset  neg_flip  name
  1,2,3     127,126,125 0           128         1         2s Complement
  65,66,67  63,62,61    64          64          1         Offset
  1,2,3     33,34,35    0           32          -1        Sign
  17,18,19  15,14,13    16          16          1
  1,2,3     65,66,67    0           64          -1        x-touch mini
*/
gint interpret_move(dt_midi_knob_t *k, gint velocity)
{
    if (k)
    {
      gint calculated_positive = (velocity - k->pos_offset);
      gint calculated_negative = (velocity - k->neg_offset) * k->neg_flip;
      if ( (calculated_positive < 0) ||
           ( (calculated_negative < 0) &&
             (calculated_positive > -1 * calculated_negative) ) )
        return calculated_negative;
      else
        return calculated_positive;
    }
    else
    {
      return 0;
    }
}

//  Currently just aggregates one channel/key combination 
//  and sends when changing to different key. This might still
//  cause flooding if multiple knobs are turned simultaneously
//  and alternating codes are received.
//  To deal with this would require maintaining a list of currently
//  changed keys and send all at end. Probably not worth extra complexity,
//  Since it would still mean sending multiple updates in one iteration 
//  which could cause flooding anyway.
void aggregate_and_set_slider(ControllerMidi *midi,
                              gint channel, gint key, gint velocity)
{
  if (!midi->config_loaded)
  {
    midi_load(midi);

    midi->config_loaded = TRUE;
  }

  if ((channel == midi->stored_channel) && (key == midi->stored_key))
  {
    if (midi->stored_knob == NULL || midi->stored_knob->absolute)
        midi->accum = velocity; // override; just use last one
    else
        midi->accum += interpret_move(midi->stored_knob, velocity);
  }
  else
  {
    if (midi->stored_channel != -1)
    {
      if (midi->stored_knob)
      {
        GtkWidget *w = midi->stored_knob->accelerator->widget;

        if (w)
        {
          if (midi->stored_knob->absolute)
          {
            float wmin = dt_bauhaus_slider_get_soft_min(w);
            float wmax = dt_bauhaus_slider_get_soft_max(w);
            float wval = dt_bauhaus_slider_get(w);

            int location = round((wval-wmin)/(wmax-wmin)*127);

            if (abs(midi->accum - location) > 4)
            {
              midi->stored_knob->locked = FALSE;
            }

            if (midi->stored_knob->locked ||
                abs(midi->accum - location) <= 1)
                
            {
              if (!midi->stored_knob->locked)
              {
                midi->stored_knob->locked = TRUE;
                dt_control_log((">%s/%s<"), 
                                  DT_BAUHAUS_WIDGET(w)->module->name(),
                                  DT_BAUHAUS_WIDGET(w)->label);
              }
                
              dt_bauhaus_slider_set(w, wmin + (wmax-wmin)*midi->accum/127);
            }
            else
            {
              gchar *left_text  = g_strnfill(MAX(1, midi->accum-location)-1,'<');
              gchar *right_text = g_strnfill(MAX(1, location-midi->accum)-1,'>');
              
              dt_control_log(("%s %s/%s %s"), 
                              left_text, DT_BAUHAUS_WIDGET(w)->module->name(),
                              DT_BAUHAUS_WIDGET(w)->label, right_text);
              
              g_free(left_text);
              g_free(right_text);

              // if one knob is out of sync, all on same controller may need syncing
              refresh_knobs_to_controller(midi);
            }
          }
          else
          {
            float v = dt_bauhaus_slider_get(w);
            float s = dt_bauhaus_slider_get_step(w);
            dt_bauhaus_slider_set(w, 
                  v + s * midi->stored_knob->acceleration * midi->accum);
          }
        }
      }
    }

    if (channel != -1)
    {
      if (mapping_widget)
      {
        // link knob to widget and set encoding type

        if (midi->mapping_channel == -1)
        {
          midi->mapping_channel  = channel;
          midi->mapping_key      = key;
          midi->mapping_velocity = velocity;

          dt_control_hinter_message
                (darktable.control, _("configure midi controller knob:\n"
                                      "   carefully turn same knob down one notch"));
        }
        else
        {
          dt_control_hinter_message (darktable.control, "");

          GSList *al = darktable.control->dynamic_accelerator_list;
          dt_accel_dynamic_t *da = NULL ;
          while(al)
          {
            da = (dt_accel_dynamic_t *)al->data;
            if (da->widget == mapping_widget)
              break;

            al = g_slist_next(al);
          }

          if ((channel == midi->mapping_channel) && (key == midi->mapping_key))
          {
            // store new mapping in table, overriding existing

            dt_midi_knob_t *new_knob = NULL;

            GSList *l = midi->mapping_list;
            while(l)
            {
              dt_midi_knob_t *d = (dt_midi_knob_t *)l->data;
              if ((d->group > midi->group) |
                  ((d->group == midi->group) && 
                   ((d->channel > channel) |
                    ((d->channel == channel) && (d->key >= key)))))
              {
                if ((d->group == midi->group) && 
                    (d->channel == channel) && 
                    (d->key == key))
                {
                  new_knob = d;
                }
                else
                {
                  new_knob = (dt_midi_knob_t *)g_malloc(sizeof(dt_midi_knob_t));
                  midi->mapping_list = g_slist_insert_before(midi->mapping_list, l, new_knob);
                }
                break;
              }
              l = g_slist_next(l);
            }
            if (!new_knob)
            {
              new_knob = (dt_midi_knob_t *)g_malloc(sizeof(dt_midi_knob_t));
              midi->mapping_list = g_slist_append(midi->mapping_list, new_knob);
            }
            new_knob->group = midi->group;
            new_knob->channel = channel;
            new_knob->key = key;
            new_knob->acceleration = 1;
            new_knob->accelerator = da;
            if (midi->mapping_velocity - velocity == 1)
            {
              new_knob->absolute = TRUE;
              new_knob->locked = FALSE;

              new_knob->pos_offset = 0;
              new_knob->neg_flip = 0;
              new_knob->neg_offset = 0;
            }
            else
            {
              new_knob->absolute = FALSE;

              new_knob->pos_offset = midi->mapping_velocity - 1;
              if ((velocity & 0xF) == 0xF)
              {
                new_knob->neg_flip = 1;
                new_knob->neg_offset = velocity + 1;
              }
              else
              {
                new_knob->neg_flip = -1;
                new_knob->neg_offset = velocity - 1;
              }
            }
          
            midi_save(midi);
          }

          midi->mapping_channel = -1;
          knob_config_mode = FALSE;
          mapping_widget = NULL;
        }
        
        channel = -1;
        key = -1;
      }
      else
      {
        midi->stored_knob = NULL;

        GSList *l = midi->mapping_list;
        while(l)
        {
          dt_midi_knob_t *d = (dt_midi_knob_t *)l->data;
          if ((d->group > midi->group) |
              ((d->group == midi->group) && 
                ((d->channel > channel) |
                ((d->channel == channel) && (d->key >= key)))))
          {
            if ((d->group == midi->group) &&
                (d->channel == channel) && 
                (d->key == key))
            {
              midi->stored_knob = d;
            }
            break;
          }
          l = g_slist_next(l);
        }

        if (midi->stored_knob == NULL)
        {
          dt_control_log(_("knob %d on channel %d not mapped in group %d"), 
                           key, channel, midi->group);
        }
        else if (midi->stored_knob->absolute)
        {
          midi->accum = velocity;
        }
        else
        {
          midi->accum = interpret_move(midi->stored_knob, velocity);
          
          if (knob_config_mode)
          {
            // configure acceleration setting
            if (midi->accum > 0)
            {
              midi->stored_knob->acceleration *= 2;
            }
            else
            {
              midi->stored_knob->acceleration /= 2;
            }

            dt_control_log(_("knob acceleration %.2f"), midi->stored_knob->acceleration);

            midi_save(midi);

            channel = -1;
            key = -1;
          }
        }

        knob_config_mode = FALSE;
      }     
    }

    midi->stored_channel = channel;
    midi->stored_key = key;
  }
}

void select_page(ControllerMidi *midi, char note)
{
  aggregate_and_set_slider(midi, -1, -1, 0);

  if (midi->group_switch_key == -1)
  {
    midi->group_switch_key = note;
  }
  midi->group = note - midi->group_switch_key;

  char *help_text = g_strdup_printf("MIDI key group %d:\n", midi->group);

  GSList *l = midi->mapping_list;
  while(l)
  {
    dt_midi_knob_t *d = (dt_midi_knob_t *)l->data;
    if (d->group > midi->group)
    {
      break;
    }
    dt_bauhaus_widget_t *w = DT_BAUHAUS_WIDGET(d->accelerator->widget);
    
    if (d->group == midi->group && w != NULL)
    {
      char *tmp;
      if (!strstr(w->module->name(), w->label))
      {
        tmp = g_strdup_printf("%s%d:%s/%s  ", 
                               help_text, d->key, w->module->name(), w->label);
      }
      else
      {
        tmp = g_strdup_printf("%s%d:%s  ", 
                               help_text, d->key, w->label);
      }
      g_free(help_text);
      help_text = tmp;
    }
    l = g_slist_next(l);
  }

  dt_control_hinter_message(darktable.control, help_text);
  
  g_free(help_text);
}

void select_page_off(ControllerMidi *midi)
{
  dt_control_hinter_message(darktable.control, _(""));
}

gboolean midi_read_event (GIOChannel   *io,
                          GIOCondition  cond,
                          gpointer      data)
{
  ControllerMidi *midi = (ControllerMidi*) data;
  GIOStatus       status;
  GError         *error = NULL;
  guchar          buf[0x3F];
  gsize           size;
  gint            pos = 0;

  status = g_io_channel_read_chars (io,
                                    (gchar *) buf,
                                    sizeof (buf), &size,
                                    &error);

  switch (status)
  {
    case G_IO_STATUS_ERROR:
    case G_IO_STATUS_EOF:
      g_source_remove (midi->io_id);
      midi->io_id = 0;

      g_io_channel_unref (midi->io);
      midi->io = NULL;

      if (error)
      {
        g_clear_error (&error);
      }
      return FALSE;
      break;

    case G_IO_STATUS_AGAIN:
      return TRUE;

    default:
      break;
  }

  if (!midi->name_queried)
  {
      // Send Universal Device Inquiry message 
      char inquiry[6] = "\xF0\x7E\x7F\x06\x01\xF7";
      int tmp = write(g_io_channel_unix_get_fd (midi->io), inquiry, 6);
      tmp++;

      midi->name_queried = TRUE;
      return TRUE; // ignore rest of input
  }

  while (pos < size)
  {
    if (buf[pos] & 0x80)  /* status byte */
    {
      if (buf[pos] >= 0xf8)  /* realtime messages */
      {
        switch (buf[pos])
        {
          case 0xf8:  /* timing clock   */
          case 0xf9:  /* (undefined)    */
          case 0xfa:  /* start          */
          case 0xfb:  /* continue       */
          case 0xfc:  /* stop           */
          case 0xfd:  /* (undefined)    */
          case 0xfe:  /* active sensing */
          case 0xff:  /* system reset   */
            break;
        }
      }
      else
      {
        midi->swallow = FALSE;  /* any status bytes ends swallowing */

        if (buf[pos] >= 0xf0)  /* system messages */
        {
          switch (buf[pos])
          {
            case 0xf0:  /* sysex start */
              midi->swallow = TRUE;

              D (g_print ("MIDI: sysex start\n"));

              // Look for:
              // F0 7E [??] 06 02 Universal Device Reply
              // In response to f0 7e 7f 06 01 f7 Universal Device Inquiry
              // that we sent earlier.
              if ( (buf[pos+1] & 0xFE) == 0x7E &&
                   (buf[pos+3] == 6) )
                pos++;

              if ( buf[pos+2] == 6 && buf[pos+3] == 2 )
              {
                //00 00 0E Alesis Manufacturer ID
                //0E 00    QS Family ID, LSB first
                //0x 00    QS Family Member, LSB first
                //xx xx xx xx Software revision, ASCI (ex. 30 31 30 30 = '0100' = 1.00)
                //F7 End-Of-Exclusive        
                //Arturia Beatstep responds with:
                //7e 00 06 02 00 20 6b 02 00 06 00 03 00 02 01
                //turn this into string 00206B_0002_0006
                midi->model_name = g_strdup_printf(
                                "%02X%02X%02X_%02X%02X_%02X%02X",
                                buf[pos+4],buf[pos+5],buf[pos+6],
                                buf[pos+8],buf[pos+7],
                                buf[pos+10],buf[pos+9]);
              }
              break;

            case 0xf1:              /* time code   */
              midi->swallow = TRUE; /* type + data */

              D (g_print ("MIDI: time code\n"));
              break;

            case 0xf2:              /* song position */
              midi->swallow = TRUE; /* lsb + msb     */

              D (g_print ("MIDI: song position\n"));
              break;

            case 0xf3:              /* song select */
              midi->swallow = TRUE; /* song number */

              D (g_print ("MIDI: song select\n"));
              break;

            case 0xf4:  /* (undefined) */
            case 0xf5:  /* (undefined) */
              D (g_print ("MIDI: undefined system message\n"));
              break;

            case 0xf6:  /* tune request */
              D (g_print ("MIDI: tune request\n"));
              break;

            case 0xf7:  /* sysex end */
              D (g_print ("MIDI: sysex end\n"));
              break;
          }
        }
        else  /* channel messages */
        {
          midi->command = buf[pos] >> 4;
          midi->channel = buf[pos] & 0xf;

          /* reset running status */
          midi->key      = -1;
          midi->velocity = -1;
          midi->msb      = -1;
          midi->lsb      = -1;
        }
      }

      pos++;  /* status byte consumed */
      continue;
    }

    if (midi->swallow)
    {
      pos++;  /* consume any data byte */
      continue;
    }

    switch (midi->command)
    {
      case 0x8:  /* note off   */
      case 0x9:  /* note on    */
      case 0xa:  /* aftertouch */

        if (midi->key == -1)
        {
          midi->key = buf[pos++];  /* key byte consumed */
          continue;
        }

        if (midi->velocity == -1)
          midi->velocity = buf[pos++];  /* velocity byte consumed */

        /* note on with velocity = 0 means note off */
        if (midi->command == 0x9 && midi->velocity == 0x0)
          midi->command = 0x8;

        if (midi->command == 0x9)
        {
          D (g_print ("MIDI (ch %02d): note on  (%02x vel %02x)\n",
                      midi->channel, midi->key, midi->velocity));

          select_page(midi, midi->key);
        }
        else if (midi->command == 0x8)
        {
          D (g_print ("MIDI (ch %02d): note off (%02x vel %02x)\n",
                      midi->channel, midi->key, midi->velocity));

          select_page_off(midi);
        }
        else
        {
          D (g_print ("MIDI (ch %02d): polyphonic aftertouch (%02x pressure %02x)\n",
                      midi->channel, midi->key, midi->velocity));
        }

        midi->key      = -1;
        midi->velocity = -1;
        break;

      case 0xb:  /* controllers, sustain */

        if (midi->key == -1)
        {
          midi->key = buf[pos++];
          continue;
        }

        if (midi->velocity == -1)
          midi->velocity = buf[pos++];

        D (g_print ("MIDI (ch %02d): controller %d (value %d)\n",
                    midi->channel, midi->key, midi->velocity));

        aggregate_and_set_slider(midi, midi->channel, midi->key, midi->velocity);

        midi->key      = -1;
        midi->velocity = -1;
        break;

      case 0xc:  /* program change */
        midi->key = buf[pos++];

        D (g_print ("MIDI (ch %02d): program change (%d)\n",
                    midi->channel, midi->key));

        midi->key = -1;
        break;

      case 0xd:  /* channel key pressure */
        midi->velocity = buf[pos++];

        D (g_print ("MIDI (ch %02d): channel aftertouch (%d)\n",
                    midi->channel, midi->velocity));

        midi->velocity = -1;
        break;

      case 0xe:  /* pitch bend */
        if (midi->lsb == -1)
        {
          midi->lsb = buf[pos++];
          continue;
        }

        if (midi->msb == -1)
          midi->msb = buf[pos++];

        midi->velocity = midi->lsb | (midi->msb << 7);

        D (g_print ("MIDI (ch %02d): pitch (%d)\n",
                    midi->channel, midi->velocity));

        midi->msb      = -1;
        midi->lsb      = -1;
        midi->velocity = -1;
        break;
    }
  }

  aggregate_and_set_slider(midi, -1, -1, 0);

  return TRUE;
}

#ifdef HAVE_ALSA
static gboolean midi_alsa_prepare (GSource *source,
                                   gint    *timeout)
{
  ControllerMidi *midi = (ControllerMidi*) ((GAlsaSource *) source)->controller;
  gboolean        ready;

  ready = snd_seq_event_input_pending (midi->sequencer, 1) > 0;
  *timeout = ready ? 1 : 10;

  return ready;
}

static gboolean midi_alsa_check (GSource *source)
{
  ControllerMidi *midi = (ControllerMidi*) ((GAlsaSource *) source)->controller;

  return snd_seq_event_input_pending (midi->sequencer, 1) > 0;
}

static gboolean midi_alsa_dispatch (GSource     *source,
                                    GSourceFunc  callback,
                                    gpointer     user_data)
{
  ControllerMidi *midi = (ControllerMidi*)((GAlsaSource *) source)->controller;

  snd_seq_event_t       *event;
  snd_seq_client_info_t *client_info;

  gboolean return_value = FALSE;
  while ( snd_seq_event_input_pending (midi->sequencer, 1) > 0 )
  {
    return_value = TRUE;

    snd_seq_event_input (midi->sequencer, &event);

    if (event->type == SND_SEQ_EVENT_NOTEON &&
        event->data.note.velocity == 0)
    {
      event->type = SND_SEQ_EVENT_NOTEOFF;
    }

    switch (event->type)
    {
      case SND_SEQ_EVENT_NOTEON:
        select_page(midi, event->data.note.note);
        break;

      case SND_SEQ_EVENT_NOTEOFF:
        select_page_off(midi);
        break;

      case SND_SEQ_EVENT_CONTROLLER:
        aggregate_and_set_slider(midi, event->data.control.channel, 
                                       event->data.control.param, 
                                       event->data.control.value);
        break;

      case SND_SEQ_EVENT_PORT_SUBSCRIBED:
        snd_seq_client_info_malloc(&client_info);
        snd_seq_get_any_client_info (midi->sequencer,
                                     event->data.connect.sender.client,
                                     client_info);

        if (g_strcmp0(snd_seq_client_info_get_name (client_info),"darktable"))
        {
          midi->model_name = g_strdup(snd_seq_client_info_get_name (client_info));
          midi->config_loaded = FALSE;
        }

        snd_seq_client_info_free(client_info);
        break;

      case SND_SEQ_EVENT_PORT_UNSUBSCRIBED:
        break;

      default:
        break;
    }
  }

  aggregate_and_set_slider(midi, -1, -1, -1);

  return return_value;
}
#endif /* HAVE_ALSA */

static gboolean _lib_midi_configure_knob_callback(GtkAccelGroup *accel_group,
                                                  GObject *acceleratable, guint keyval,
                                                  GdkModifierType modifier, gpointer data)
{
//  dt_lib_module_t *self = (dt_lib_module_t *)data;
  if (!knob_config_mode && !mapping_widget)
  {
    knob_config_mode = TRUE;

    dt_control_hinter_message
            (darktable.control, _("configure midi controller knob\n"
                                  "   move knob up or down to change acceleration. move slider first to connect to knob."));
  }
  else
  {
    knob_config_mode = FALSE;
    mapping_widget = NULL;

    dt_control_hinter_message
            (darktable.control, (""));
  }

  return TRUE;
}

void init_key_accels(dt_lib_module_t *self)
{
  dt_accel_register_lib(self, NC_("accel", "configure knob"), GDK_KEY_M, GDK_CONTROL_MASK);
}

void connect_key_accels(dt_lib_module_t *self)
{
  dt_accel_connect_lib(self, "configure knob",
                     g_cclosure_new(G_CALLBACK(_lib_midi_configure_knob_callback), self, NULL));
}

void gui_init(dt_lib_module_t *self)
{
  dt_lib_midi_t *d = (dt_lib_midi_t *)g_malloc0(sizeof(dt_lib_midi_t));
  d->Controllers = NULL;
  d->SignalsConnected = FALSE;
  self->data = (void *)d;

  gchar **devices = g_strsplit_set(midi_devices,",",-1);
  gchar **cur_device = devices;

  while (*cur_device)
  {
    ControllerMidi *midi = (ControllerMidi *)g_malloc0(sizeof(ControllerMidi));

    controller_midi_init (midi);
    if (midi_set_device (midi, *cur_device))
    {
      d->Controllers = g_slist_append(d->Controllers, midi);
    }
    else
    {
      g_free(midi);
    }

    cur_device++;
  }
  g_strfreev(devices);

  if (d->Controllers)
  {
    dt_control_signal_connect(darktable.signals, DT_SIGNAL_VIEWMANAGER_VIEW_CHANGED,
                                G_CALLBACK(connect_slider_changed_callbacks), self->data);

    dt_control_signal_connect(darktable.signals, DT_SIGNAL_DEVELOP_IMAGE_CHANGED,
                              G_CALLBACK(image_changed), self->data);
  }
}

void gui_cleanup(dt_lib_module_t *self)
{
  dt_control_signal_disconnect(darktable.signals, 
                               G_CALLBACK(connect_slider_changed_callbacks), self->data);

  dt_control_signal_disconnect(darktable.signals, 
                               G_CALLBACK(image_changed), self->data);
  g_free(self->data);
  self->data = NULL;
}

// modelines: These editor modelines have been set for all relevant files by tools/update_modelines.sh
// vim: shiftwidth=2 expandtab tabstop=2 cindent
// kate: tab-indents: off; indent-width 2; replace-tabs on; indent-mode cstyle; remove-trailing-spaces modified;
