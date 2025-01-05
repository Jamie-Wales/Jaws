#include <math.h>
#include <portaudio.h>
#include <stdlib.h>

// Export for macOS dylib
#define EXPORT __attribute__((visibility("default")))

// Global state to keep track of audio stream
typedef struct {
    PaStream* stream;
    double frequency;
    double amplitude;
    double phase;
} AudioState;

static AudioState audio_state = { NULL, 440.0, 0.5, 0.0 };

static int audioCallback(
    const void* inputBuffer,
    void* outputBuffer,
    unsigned long framesPerBuffer,
    const PaStreamCallbackTimeInfo* timeInfo,
    PaStreamCallbackFlags statusFlags,
    void* userData)
{
    float* out = (float*)outputBuffer;
    AudioState* state = (AudioState*)userData;

    for (unsigned long i = 0; i < framesPerBuffer; i++) {
        out[i] = state->amplitude * sin(2 * M_PI * state->frequency * state->phase);
        state->phase += 1.0 / 44100;
        if (state->phase >= 1.0)
            state->phase -= 1.0;
    }
    return paContinue;
}

// Simple C API for FFI
EXPORT int pa_initialize(void)
{
    if (audio_state.stream != NULL) {
        return 1; // Already initialized
    }

    PaError err = Pa_Initialize();
    if (err != paNoError)
        return 0;

    err = Pa_OpenDefaultStream(
        &audio_state.stream,
        0, // no input channels
        1, // mono output
        paFloat32, // sample format
        44100, // sample rate
        256, // frames per buffer
        audioCallback,
        &audio_state); // Pass our state as user data

    return (err == paNoError) ? 1 : 0;
}

EXPORT int pa_start(void)
{
    if (!audio_state.stream)
        return 0;
    return (Pa_StartStream(audio_state.stream) == paNoError) ? 1 : 0;
}

EXPORT int pa_stop(void)
{
    if (!audio_state.stream)
        return 0;
    return (Pa_StopStream(audio_state.stream) == paNoError) ? 1 : 0;
}

EXPORT void pa_set_frequency(double freq)
{
    audio_state.frequency = freq;
}

EXPORT void pa_set_amplitude(double amp)
{
    audio_state.amplitude = amp > 1.0 ? 1.0 : (amp < 0.0 ? 0.0 : amp);
}

EXPORT void pa_cleanup(void)
{
    if (audio_state.stream) {
        Pa_CloseStream(audio_state.stream);
        audio_state.stream = NULL;
    }
    Pa_Terminate();
}
