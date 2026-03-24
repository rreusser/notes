

// TypeScript declarations for @stdlib/lapack/base/zlaqr2

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Complex aggressive early deflation (non-recursive)
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
		SH: Float64Array,
		strideSH: number,
		offsetSH: number,
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
* Complex aggressive early deflation (non-recursive)
*/
declare var zlaqr2: Routine;

export = zlaqr2;
