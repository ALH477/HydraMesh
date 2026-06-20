/* ------------------------------------------------------------
author: "DeMoD LLC"
license: "LGPL-3.0-only"
name: "DCF RF Modulator"
version: "0.3.0"
Code generated with Faust 2.83.1 (https://faust.grame.fr)
Compilation options: -lang c -fpga-mem-th 4 -ct 1 -cn DcfRfModulator -es 1 -mcd 16 -mdd 1024 -mdy 33 -single -ftz 0
------------------------------------------------------------ */

#ifndef  __DcfRfModulator_H__
#define  __DcfRfModulator_H__

#ifndef FAUSTFLOAT
#define FAUSTFLOAT float
#endif 


#ifdef __cplusplus
extern "C" {
#endif

#if defined(_WIN32)
#define RESTRICT __restrict
#else
#define RESTRICT __restrict__
#endif

#include <math.h>
#include <stdint.h>
#include <stdlib.h>


#ifndef FAUSTCLASS 
#define FAUSTCLASS DcfRfModulator
#endif

#ifdef __APPLE__ 
#define exp10f __exp10f
#define exp10 __exp10
#endif
static inline int max_i(int a, int b) { return (a > b) ? a : b; }
static inline int min_i(int a, int b) { return (a < b) ? a : b; }

typedef struct {
	float fRec0[2];
	int fSampleRate;
} DcfRfModulator;

DcfRfModulator* newDcfRfModulator() { 
	DcfRfModulator* dsp = (DcfRfModulator*)calloc(1, sizeof(DcfRfModulator));
	return dsp;
}

void deleteDcfRfModulator(DcfRfModulator* dsp) { 
	free(dsp);
}

void metadataDcfRfModulator(MetaGlue* m) { 
	m->declare(m->metaInterface, "author", "DeMoD LLC");
	m->declare(m->metaInterface, "compile_options", "-lang c -fpga-mem-th 4 -ct 1 -cn DcfRfModulator -es 1 -mcd 16 -mdd 1024 -mdy 33 -single -ftz 0");
	m->declare(m->metaInterface, "filename", "dcf_rf_modulator.dsp");
	m->declare(m->metaInterface, "license", "LGPL-3.0-only");
	m->declare(m->metaInterface, "maths.lib/author", "GRAME");
	m->declare(m->metaInterface, "maths.lib/copyright", "GRAME");
	m->declare(m->metaInterface, "maths.lib/license", "LGPL with exception");
	m->declare(m->metaInterface, "maths.lib/name", "Faust Math Library");
	m->declare(m->metaInterface, "maths.lib/version", "2.9.0");
	m->declare(m->metaInterface, "name", "DCF RF Modulator");
	m->declare(m->metaInterface, "version", "0.3.0");
}

int getSampleRateDcfRfModulator(DcfRfModulator* RESTRICT dsp) {
	return dsp->fSampleRate;
}

int getNumInputsDcfRfModulator(DcfRfModulator* RESTRICT dsp) {
	return 1;
}
int getNumOutputsDcfRfModulator(DcfRfModulator* RESTRICT dsp) {
	return 2;
}

void classInitDcfRfModulator(int sample_rate) {
}

void instanceResetUserInterfaceDcfRfModulator(DcfRfModulator* dsp) {
}

void instanceClearDcfRfModulator(DcfRfModulator* dsp) {
	/* C99 loop */
	{
		int l0;
		for (l0 = 0; l0 < 2; l0 = l0 + 1) {
			dsp->fRec0[l0] = 0.0f;
		}
	}
}

void instanceConstantsDcfRfModulator(DcfRfModulator* dsp, int sample_rate) {
	dsp->fSampleRate = sample_rate;
}
	
void instanceInitDcfRfModulator(DcfRfModulator* dsp, int sample_rate) {
	instanceConstantsDcfRfModulator(dsp, sample_rate);
	instanceResetUserInterfaceDcfRfModulator(dsp);
	instanceClearDcfRfModulator(dsp);
}

void initDcfRfModulator(DcfRfModulator* dsp, int sample_rate) {
	classInitDcfRfModulator(sample_rate);
	instanceInitDcfRfModulator(dsp, sample_rate);
}

void buildUserInterfaceDcfRfModulator(DcfRfModulator* dsp, UIGlue* ui_interface) {
	ui_interface->openVerticalBox(ui_interface->uiInterface, "DCF RF Modulator");
	ui_interface->closeBox(ui_interface->uiInterface);
}

void computeDcfRfModulator(DcfRfModulator* dsp, int count, FAUSTFLOAT** RESTRICT inputs, FAUSTFLOAT** RESTRICT outputs) {
	FAUSTFLOAT* input0 = inputs[0];
	FAUSTFLOAT* output0 = outputs[0];
	FAUSTFLOAT* output1 = outputs[1];
	/* C99 loop */
	{
		int i0;
		for (i0 = 0; i0 < count; i0 = i0 + 1) {
			dsp->fRec0[0] = (((int)((float)(input0[i0]))) ? 0.25f : -0.25f) + dsp->fRec0[1];
			float fTemp0 = 6.2831855f * dsp->fRec0[0];
			output0[i0] = (FAUSTFLOAT)(cosf(fTemp0));
			output1[i0] = (FAUSTFLOAT)(sinf(fTemp0));
			dsp->fRec0[1] = dsp->fRec0[0];
		}
	}
}

#ifdef __cplusplus
}
#endif

#endif
