

// TypeScript declarations for @stdlib/lapack/base/dlaqr2

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Performs aggressive early deflation on an upper Hessenberg matrix
	*/
	(
		wantt: boolean,
		wantz: boolean,
		N: number,
		ktop: number,
		kbot: number,
		nw: number,
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
		ns: number,
		nd: number,
		SR: Float64Array,
		strideSR: number,
		offsetSR: number,
		SI: Float64Array,
		strideSI: number,
		offsetSI: number,
		V: Float64Array,
		strideV1: number,
		strideV2: number,
		offsetV: number,
		nh: number,
		T: Float64Array,
		strideT1: number,
		strideT2: number,
		offsetT: number,
		nv: number,
		WV: Float64Array,
		strideWV1: number,
		strideWV2: number,
		offsetWV: number,
		WORK: Float64Array,
		strideWORK: number,
		offsetWORK: number,
		lwork: number
	): Float64Array;
}

/**
* Performs aggressive early deflation on an upper Hessenberg matrix
*/
declare var dlaqr2: Routine;

export = dlaqr2;
