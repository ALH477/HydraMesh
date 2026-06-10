/* ------------------------------------------------------------
name: "dcf_pm_codec"
Code generated with Faust 2.83.1 (https://faust.grame.fr)
Compilation options: -lang c -fpga-mem-th 4 -ct 1 -cn DcfPmCodec -es 1 -mcd 16 -mdd 1024 -mdy 33 -single -ftz 0
------------------------------------------------------------ */

#ifndef  __DcfPmCodec_H__
#define  __DcfPmCodec_H__

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

typedef struct {
	int iVec0[2];
	int iRec0[2];
} DcfPmCodecSIG0;

static DcfPmCodecSIG0* newDcfPmCodecSIG0() { return (DcfPmCodecSIG0*)calloc(1, sizeof(DcfPmCodecSIG0)); }
static void deleteDcfPmCodecSIG0(DcfPmCodecSIG0* dsp) { free(dsp); }

int getNumInputsDcfPmCodecSIG0(DcfPmCodecSIG0* RESTRICT dsp) {
	return 0;
}
int getNumOutputsDcfPmCodecSIG0(DcfPmCodecSIG0* RESTRICT dsp) {
	return 1;
}

static void instanceInitDcfPmCodecSIG0(DcfPmCodecSIG0* dsp, int sample_rate) {
	/* C99 loop */
	{
		int l0;
		for (l0 = 0; l0 < 2; l0 = l0 + 1) {
			dsp->iVec0[l0] = 0;
		}
	}
	/* C99 loop */
	{
		int l1;
		for (l1 = 0; l1 < 2; l1 = l1 + 1) {
			dsp->iRec0[l1] = 0;
		}
	}
}

static void fillDcfPmCodecSIG0(DcfPmCodecSIG0* dsp, int count, float* table) {
	/* C99 loop */
	{
		int i1;
		for (i1 = 0; i1 < count; i1 = i1 + 1) {
			dsp->iVec0[0] = 1;
			dsp->iRec0[0] = (dsp->iVec0[1] + dsp->iRec0[1]) % 65536;
			table[i1] = sinf(9.58738e-05f * (float)(dsp->iRec0[0]));
			dsp->iVec0[1] = dsp->iVec0[0];
			dsp->iRec0[1] = dsp->iRec0[0];
		}
	}
}

typedef struct {
	int iVec2[2];
	int iRec2[2];
} DcfPmCodecSIG1;

static DcfPmCodecSIG1* newDcfPmCodecSIG1() { return (DcfPmCodecSIG1*)calloc(1, sizeof(DcfPmCodecSIG1)); }
static void deleteDcfPmCodecSIG1(DcfPmCodecSIG1* dsp) { free(dsp); }

int getNumInputsDcfPmCodecSIG1(DcfPmCodecSIG1* RESTRICT dsp) {
	return 0;
}
int getNumOutputsDcfPmCodecSIG1(DcfPmCodecSIG1* RESTRICT dsp) {
	return 1;
}

static void instanceInitDcfPmCodecSIG1(DcfPmCodecSIG1* dsp, int sample_rate) {
	/* C99 loop */
	{
		int l4;
		for (l4 = 0; l4 < 2; l4 = l4 + 1) {
			dsp->iVec2[l4] = 0;
		}
	}
	/* C99 loop */
	{
		int l5;
		for (l5 = 0; l5 < 2; l5 = l5 + 1) {
			dsp->iRec2[l5] = 0;
		}
	}
}

static void fillDcfPmCodecSIG1(DcfPmCodecSIG1* dsp, int count, float* table) {
	/* C99 loop */
	{
		int i2;
		for (i2 = 0; i2 < count; i2 = i2 + 1) {
			dsp->iVec2[0] = 1;
			dsp->iRec2[0] = (dsp->iVec2[1] + dsp->iRec2[1]) % 65536;
			table[i2] = cosf(9.58738e-05f * (float)(dsp->iRec2[0]));
			dsp->iVec2[1] = dsp->iVec2[0];
			dsp->iRec2[1] = dsp->iRec2[0];
		}
	}
}

static float ftbl0DcfPmCodecSIG0[65536];
static float ftbl1DcfPmCodecSIG1[65536];

#ifndef FAUSTCLASS 
#define FAUSTCLASS DcfPmCodec
#endif

#ifdef __APPLE__ 
#define exp10f __exp10f
#define exp10 __exp10
#endif
static inline int max_i(int a, int b) { return (a > b) ? a : b; }
static inline int min_i(int a, int b) { return (a < b) ? a : b; }

typedef struct {
	int iVec1[2];
	FAUSTFLOAT fEntry0;
	FAUSTFLOAT fEntry1;
	int fSampleRate;
	float fConst0;
	float fRec1[2];
	FAUSTFLOAT fEntry2;
	FAUSTFLOAT fEntry3;
	float fRec3[2];
	FAUSTFLOAT fEntry4;
	FAUSTFLOAT fEntry5;
} DcfPmCodec;

DcfPmCodec* newDcfPmCodec() { 
	DcfPmCodec* dsp = (DcfPmCodec*)calloc(1, sizeof(DcfPmCodec));
	return dsp;
}

void deleteDcfPmCodec(DcfPmCodec* dsp) { 
	free(dsp);
}

void metadataDcfPmCodec(MetaGlue* m) { 
	m->declare(m->metaInterface, "basics.lib/name", "Faust Basic Element Library");
	m->declare(m->metaInterface, "basics.lib/version", "1.22.0");
	m->declare(m->metaInterface, "compile_options", "-lang c -fpga-mem-th 4 -ct 1 -cn DcfPmCodec -es 1 -mcd 16 -mdd 1024 -mdy 33 -single -ftz 0");
	m->declare(m->metaInterface, "filename", "dcf_pm_codec.dsp");
	m->declare(m->metaInterface, "maths.lib/author", "GRAME");
	m->declare(m->metaInterface, "maths.lib/copyright", "GRAME");
	m->declare(m->metaInterface, "maths.lib/license", "LGPL with exception");
	m->declare(m->metaInterface, "maths.lib/name", "Faust Math Library");
	m->declare(m->metaInterface, "maths.lib/version", "2.9.0");
	m->declare(m->metaInterface, "name", "dcf_pm_codec");
	m->declare(m->metaInterface, "oscillators.lib/name", "Faust Oscillator Library");
	m->declare(m->metaInterface, "oscillators.lib/version", "1.6.0");
	m->declare(m->metaInterface, "platform.lib/name", "Generic Platform Library");
	m->declare(m->metaInterface, "platform.lib/version", "1.3.0");
}

int getSampleRateDcfPmCodec(DcfPmCodec* RESTRICT dsp) {
	return dsp->fSampleRate;
}

int getNumInputsDcfPmCodec(DcfPmCodec* RESTRICT dsp) {
	return 0;
}
int getNumOutputsDcfPmCodec(DcfPmCodec* RESTRICT dsp) {
	return 1;
}

void classInitDcfPmCodec(int sample_rate) {
	DcfPmCodecSIG0* sig0 = newDcfPmCodecSIG0();
	instanceInitDcfPmCodecSIG0(sig0, sample_rate);
	fillDcfPmCodecSIG0(sig0, 65536, ftbl0DcfPmCodecSIG0);
	DcfPmCodecSIG1* sig1 = newDcfPmCodecSIG1();
	instanceInitDcfPmCodecSIG1(sig1, sample_rate);
	fillDcfPmCodecSIG1(sig1, 65536, ftbl1DcfPmCodecSIG1);
	deleteDcfPmCodecSIG0(sig0);
	deleteDcfPmCodecSIG1(sig1);
}

void instanceResetUserInterfaceDcfPmCodec(DcfPmCodec* dsp) {
	dsp->fEntry0 = (FAUSTFLOAT)(2.2e+02f);
	dsp->fEntry1 = (FAUSTFLOAT)(1.0f);
	dsp->fEntry2 = (FAUSTFLOAT)(0.0f);
	dsp->fEntry3 = (FAUSTFLOAT)(1.0f);
	dsp->fEntry4 = (FAUSTFLOAT)(0.5f);
	dsp->fEntry5 = (FAUSTFLOAT)(1.0f);
}

void instanceClearDcfPmCodec(DcfPmCodec* dsp) {
	/* C99 loop */
	{
		int l2;
		for (l2 = 0; l2 < 2; l2 = l2 + 1) {
			dsp->iVec1[l2] = 0;
		}
	}
	/* C99 loop */
	{
		int l3;
		for (l3 = 0; l3 < 2; l3 = l3 + 1) {
			dsp->fRec1[l3] = 0.0f;
		}
	}
	/* C99 loop */
	{
		int l6;
		for (l6 = 0; l6 < 2; l6 = l6 + 1) {
			dsp->fRec3[l6] = 0.0f;
		}
	}
}

void instanceConstantsDcfPmCodec(DcfPmCodec* dsp, int sample_rate) {
	dsp->fSampleRate = sample_rate;
	dsp->fConst0 = 1.0f / fminf(1.92e+05f, fmaxf(1.0f, (float)(dsp->fSampleRate)));
}
	
void instanceInitDcfPmCodec(DcfPmCodec* dsp, int sample_rate) {
	instanceConstantsDcfPmCodec(dsp, sample_rate);
	instanceResetUserInterfaceDcfPmCodec(dsp);
	instanceClearDcfPmCodec(dsp);
}

void initDcfPmCodec(DcfPmCodec* dsp, int sample_rate) {
	classInitDcfPmCodec(sample_rate);
	instanceInitDcfPmCodec(dsp, sample_rate);
}

void buildUserInterfaceDcfPmCodec(DcfPmCodec* dsp, UIGlue* ui_interface) {
	ui_interface->openVerticalBox(ui_interface->uiInterface, "dcf_pm_codec");
	ui_interface->addNumEntry(ui_interface->uiInterface, "amp", &dsp->fEntry4, (FAUSTFLOAT)0.5f, (FAUSTFLOAT)0.0f, (FAUSTFLOAT)1.0f, (FAUSTFLOAT)0.0001f);
	ui_interface->addNumEntry(ui_interface->uiInterface, "bright", &dsp->fEntry2, (FAUSTFLOAT)0.0f, (FAUSTFLOAT)0.0f, (FAUSTFLOAT)1.0f, (FAUSTFLOAT)0.0001f);
	ui_interface->addNumEntry(ui_interface->uiInterface, "env", &dsp->fEntry5, (FAUSTFLOAT)1.0f, (FAUSTFLOAT)0.0f, (FAUSTFLOAT)1.0f, (FAUSTFLOAT)0.0001f);
	ui_interface->addNumEntry(ui_interface->uiInterface, "f0", &dsp->fEntry0, (FAUSTFLOAT)2.2e+02f, (FAUSTFLOAT)2e+01f, (FAUSTFLOAT)8e+03f, (FAUSTFLOAT)0.001f);
	ui_interface->addNumEntry(ui_interface->uiInterface, "mod_index", &dsp->fEntry3, (FAUSTFLOAT)1.0f, (FAUSTFLOAT)0.0f, (FAUSTFLOAT)8.0f, (FAUSTFLOAT)0.0001f);
	ui_interface->addNumEntry(ui_interface->uiInterface, "mod_ratio", &dsp->fEntry1, (FAUSTFLOAT)1.0f, (FAUSTFLOAT)0.25f, (FAUSTFLOAT)8.0f, (FAUSTFLOAT)0.0001f);
	ui_interface->closeBox(ui_interface->uiInterface);
}

void computeDcfPmCodec(DcfPmCodec* dsp, int count, FAUSTFLOAT** RESTRICT inputs, FAUSTFLOAT** RESTRICT outputs) {
	FAUSTFLOAT* output0 = outputs[0];
	float fSlow0 = (float)(dsp->fEntry0);
	float fSlow1 = dsp->fConst0 * (float)(dsp->fEntry1) * fSlow0;
	float fSlow2 = (float)(dsp->fEntry3) * ((float)(dsp->fEntry2) + 1.0f);
	float fSlow3 = dsp->fConst0 * fSlow0;
	float fSlow4 = (float)(dsp->fEntry5) * (float)(dsp->fEntry4);
	/* C99 loop */
	{
		int i0;
		for (i0 = 0; i0 < count; i0 = i0 + 1) {
			dsp->iVec1[0] = 1;
			int iTemp0 = 1 - dsp->iVec1[1];
			float fTemp1 = ((iTemp0) ? 0.0f : fSlow1 + dsp->fRec1[1]);
			dsp->fRec1[0] = fTemp1 - floorf(fTemp1);
			float fTemp2 = fSlow2 * ftbl0DcfPmCodecSIG0[max_i(0, min_i((int)(65536.0f * dsp->fRec1[0]), 65535))];
			float fTemp3 = ((iTemp0) ? 0.0f : fSlow3 + dsp->fRec3[1]);
			dsp->fRec3[0] = fTemp3 - floorf(fTemp3);
			int iTemp4 = max_i(0, min_i((int)(65536.0f * dsp->fRec3[0]), 65535));
			output0[i0] = (FAUSTFLOAT)(fSlow4 * (ftbl0DcfPmCodecSIG0[iTemp4] * cosf(fTemp2) + ftbl1DcfPmCodecSIG1[iTemp4] * sinf(fTemp2)));
			dsp->iVec1[1] = dsp->iVec1[0];
			dsp->fRec1[1] = dsp->fRec1[0];
			dsp->fRec3[1] = dsp->fRec3[0];
		}
	}
}

#ifdef __cplusplus
}
#endif

#endif
