use std::sync::Mutex;
use std::sync::Arc;
use std::thread;

use cpal::traits::{DeviceTrait, HostTrait, StreamTrait};
use cpal::Device;
use cpal::FromSample;
use cpal::SampleFormat;
use cpal::SizedSample;
use cpal::StreamConfig;

//TODO:
//Use pulse for channel 1, 2, and 3
//Hz Formula is 131072/2048-period_value for 1 and 2
//Hz Formula is 65536/2048-period_value for 3 
//Use white for channel 4
//Use zero for no sound
use fundsp::hacker::{
  AudioUnit, constant, sine_hz, pulse, white, zero};

#[derive(Clone)]
struct Audio {
  memory: Arc<Mutex<Box<[u8; 0x10000]>>>,
  sample_rate: f64,
}

impl Audio {
  pub fn read_memory(&self, position: u16) -> u8 {
    (*self.memory.lock().unwrap())[usize::from(position)]
  }

  pub fn write_memory(&self, position: u16, data: u8) {
    (*self.memory.lock().unwrap())[usize::from(position)] = data;
  }

  pub fn audio_master_on(&self) -> bool {
    (self.read_memory(0xff26) & 0x80) > 0
  }

  pub fn channel_on(&self, channel: u8) -> bool {
    (self.read_memory(0xff26) & (0x01 << (channel-1))) > 0
  }

  pub fn channel_off(&self, channel: u8) {
    let channels = self.read_memory(0xff26);
    self.write_memory(0xff26, channels & !(0x01 << (channel-1)));
  }

  pub fn volume_on(&self, left:bool) -> bool {
    if left {
      (self.read_memory(0xff24) & 0x80) > 0
    } else {
      (self.read_memory(0xff24) & 0x08) > 0
    }
  }

  pub fn volume_amount(&self, left:bool) -> u8 {
    if left {
      (self.read_memory(0xff24) & 0x70) >> 4 
    } else {
      (self.read_memory(0xff24) & 0x07) >> 4
    }
  }

  pub fn channel_1_pace(&self) -> u8 {
      (self.read_memory(0xff10) & 0x70) >> 4
  }

  pub fn channel_1_direction(&self) -> u8 {
      (self.read_memory(0xff10) & 0x08) >> 3
  }

  pub fn channel_1_step(&self) -> u8 {
    self.read_memory(0xff10) & 0x07
  }

  pub fn wave_duty(&self, channel: u8) -> f32 {
    let action;
    if channel == 1 {
      action = (self.read_memory(0xff11) & 0xC0) >> 6;
    } else {
      action = (self.read_memory(0xff16) & 0xC0) >> 6;
    }
    if action == 0b00 {
      0.125
    } else if action == 0b01 {
      0.25
    } else if action == 0b10 {
      0.5
    } else {
      0.75
    }
  }

  pub fn initial_length_timer(&self, channel: u8) -> u8 {
    if channel == 1 {
      self.read_memory(0xff11) & 0x3F
    } else if channel == 2{
      self.read_memory(0xff16) & 0x3F
    } else if channel == 3{
      self.read_memory(0xff1b)
    } else {
      self.read_memory(0xff20) & 0x3F
    }
  }

  pub fn intital_volume(&self, channel: u8) -> u8 {
    if channel == 1 {
      (self.read_memory(0xff12) & 0xF0) >> 4
    } else {
      (self.read_memory(0xff17) & 0xF0) >> 4
    }
  }

  pub fn env_dir(&self, channel: u8) -> u8 {
    if channel == 1 {
      (self.read_memory(0xff12) & 0x08) >> 3
    } else {
      (self.read_memory(0xff17) & 0x08) >> 3
    }
  }

  pub fn sweep_pace(&self, channel: u8) -> u8 {
    if channel == 1 {
      self.read_memory(0xff12) & 0x07
    } else {
      self.read_memory(0xff17) & 0x07
    }
  }

  pub fn get_period(&self, channel: u8) -> u16{
    let low;
    let high;
    if channel == 1 {
      low = self.read_memory(0xff13);
      high = self.read_memory(0xff14) & 0x07;
    } else if channel == 2 {
      low = self.read_memory(0xff18);
      high = self.read_memory(0xff19) & 0x07;
    } else if channel == 3 {
      low = self.read_memory(0xff1d);
      high = self.read_memory(0xff1e) & 0x07;
    } else {
      low = 0;
      high = 0;
    }
    //println!("High: {high:02x}, Low: {low:04x}");
    ((high as u16) << 8) + (low as u16)
  }

  pub fn get_frequency_from_period(&self, period: u16) -> f32 {
    131072.0/(2048.0-(period as f32))
  }

  pub fn get_frequency_from_c3_period(&self, period: u16) -> f32 {
    65536.0/(2048.0-(period as f32))
  }

  pub fn periodic_sweep(&self) {
    let period = self.get_period(1);
    let bottom = 2u16.pow(self.channel_1_step().into());
    let mut new_period = period;
    if self.channel_1_direction() == 0 {
      new_period += period/bottom;
    } else {
      new_period -= period/bottom;
    } 
    self.write_memory(0xff13, (new_period & 0x00ff) as u8);
    let f14 = self.read_memory(0xff14);
    self.write_memory(0xff14, (f14 & 0xC0) + (((new_period & 0x0700) >> 8) as u8));
  }
  
  //TODO: Probable implementation
  pub fn dac(&self, on: bool) {
    if on {
      self.write_memory(0xff1A, 0x80);
    } else {
      self.write_memory(0xff1A, 0x00);      
    }
  }

  pub fn initial_length_3(&self, length: u8) {
    self.write_memory(0xff1B, length);
  }

  pub fn dump_c3(&self) {
    for i in 0x0..0x10 {
      let memloc = self.read_memory(0xff30 + i);
      println!("FF{i}: {memloc:04x}");
    }
  }

  pub fn get_next_value(&mut self, channel: u8) -> Box<dyn AudioUnit> {
    let audio_on = self.audio_master_on();
    if audio_on {
      if channel == 0 { // Test channel
        let mut music = Box::new(white());
        music.set_sample_rate(self.sample_rate);
        return music
      } else if (channel == 1 || channel == 2 ) && self.channel_on(channel){
        let duty_cycle = self.wave_duty(channel);
        let period = self.get_period(channel);
        let frequency = self.get_frequency_from_period(period);
        self.channel_off(channel);
        let mut sound = Box::new((constant(frequency) ^ constant(duty_cycle)) >> pulse());
        if channel == 1 && self.channel_1_pace() != 0{
          self.periodic_sweep();
        }
        //println!("{}", frequency);
        sound.set_sample_rate(self.sample_rate);
        //return sound
      } else if channel == 3 && self.channel_on(channel){
        let period = self.get_period(channel);
        let frequency = self.get_frequency_from_c3_period(period);
        let mut sound = Box::new(sine_hz(frequency));
        self.channel_off(channel);
        sound.set_sample_rate(self.sample_rate);
        //self.dump_c3();
        return sound
      } else if channel == 4 && self.channel_on(channel){
        let mut random = Box::new(white());
        self.channel_off(channel);
        random.set_sample_rate(self.sample_rate);
        return random
      }
    }
    //Return silence if no audio is on
    let mut silence = Box::new(zero());
    silence.set_sample_rate(self.sample_rate);
    silence
  }

}

pub fn run(memory: Arc<Mutex<Box<[u8; 0x10000]>>>) {
  let audio_graph = Box::new(sine_hz(261.6) + sine_hz(329.628) + sine_hz(391.995));
  let audio = Audio {
    memory,
    sample_rate: 0f64
  };
  run_output(audio_graph, audio);

}

fn run_output(audio_graph: Box<dyn AudioUnit>, audio: Audio) {
  let host = cpal::default_host();
  let device = host
      .default_output_device()
      .expect("failed to find a default output device");
  let config = device.default_output_config().unwrap();
  match config.sample_format() {
      SampleFormat::F32 => run_synth::<f32>(audio_graph, audio, device, config.into()),
      SampleFormat::I16 => run_synth::<i16>(audio_graph, audio, device, config.into()),
      SampleFormat::U8 => run_synth::<u8>(audio_graph, audio, device, config.into()),
      SampleFormat::U16 => run_synth::<u16>(audio_graph, audio, device, config.into()),
      _ => panic!("Unsupported format"),
  }
}

fn run_synth<T: SizedSample + FromSample<f64>>(
  mut _audio_graph: Box<dyn AudioUnit>,
  mut audio: Audio,
  device: Device,
  config: StreamConfig,
) {

    let sample_rate = config.sample_rate.0 as f64;
    audio.sample_rate = sample_rate;
    //Creates 4 channel threads that play their sound for the duration of time
    for i in 1..5 {
      let mut aclone = audio.clone();
      let conf = config.clone();
      let dev = device.clone();
      thread::spawn(move || {
        let channels = conf.channels as usize;
        let err_fn = |err| eprintln!("an error occurred on stream: {err}");
        loop {
          let mut agraph = aclone.get_next_value(i);
          let mut next_value = move || agraph.get_stereo();
          let initial_timer = 64 - aclone.initial_length_timer(i);

          let stream = dev
            .build_output_stream(
                &conf,
                move |data: &mut [T], _: &cpal::OutputCallbackInfo| {
                    write_data(data, channels, &mut next_value)
                },
                err_fn,
                None,
            )
            .unwrap();
  
          stream.play().unwrap();
          //Play for the designated period of time before concluding
          std::thread::sleep(std::time::Duration::from_millis((initial_timer as u64) * 1000 / 256));
        }
      });
    }

}

fn write_data<T: SizedSample + FromSample<f64>>(
  output: &mut [T],
  channels: usize,
  next_sample: &mut dyn FnMut() -> (f32, f32),
) {
  for frame in output.chunks_mut(channels) {
      let sample = next_sample();
      let left: T = T::from_sample(sample.0.into());
      let right: T = T::from_sample(sample.1.into());

      for (channel, sample) in frame.iter_mut().enumerate() {
          *sample = if channel & 1 == 0 { left } else { right };
      }
  }
}