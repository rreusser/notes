

// TypeScript declarations for @stdlib/lapack/base/zlaqr4

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Complex multishift QR with aggressive early deflation (non-recursive)
	*/
	(
		wantt: boolean,
		wantz: boolean,
		N: number,
		ilo: number,
		ihi: number,
		H: Float64Array,
		strideH1: number,
		strideH2: number,
		offsetH: number,
		w: Float64Array,
		strideW: number,
		offsetW: number,
		iloz: number,
		ihiz: number,
		Z: Float64Array,
		strideZ1: number,
		strideZ2: number,
		offsetZ: number,
		WORK: Float64Array,
		strideWORK: number,
		offsetWORK: number,
		lwork: number
	): Float64Array;
}

/**
* Complex multishift QR with aggressive early deflation (non-recursive)
*/
declare var zlaqr4: Routine;

export = zlaqr4;
