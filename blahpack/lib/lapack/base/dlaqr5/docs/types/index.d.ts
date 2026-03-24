

// TypeScript declarations for @stdlib/lapack/base/dlaqr5

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Performs a single small-bulge multi-shift QR sweep
	*/
	(
		wantt: boolean,
		wantz: boolean,
		kacc22: number,
		N: number,
		ktop: number,
		kbot: number,
		nshfts: number,
		SR: Float64Array,
		strideSR: number,
		offsetSR: number,
		SI: Float64Array,
		strideSI: number,
		offsetSI: number,
		H: Float64Array,
		strideH1: number,
		strideH2: number,
		offsetH: number,
		iloz: number,
		ihiz: number,
		Z: Float64Array,
		strideZ1: number,
		strideZ2: number,
		offsetZ: number,
		V: Float64Array,
		strideV1: number,
		strideV2: number,
		offsetV: number,
		U: Float64Array,
		strideU1: number,
		strideU2: number,
		offsetU: number,
		nv: number,
		WV: Float64Array,
		strideWV1: number,
		strideWV2: number,
		offsetWV: number,
		nh: number,
		WH: Float64Array,
		strideWH1: number,
		strideWH2: number,
		offsetWH: number
	): Float64Array;
}

/**
* Performs a single small-bulge multi-shift QR sweep
*/
declare var dlaqr5: Routine;

export = dlaqr5;
