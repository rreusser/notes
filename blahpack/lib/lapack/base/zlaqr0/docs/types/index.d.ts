

// TypeScript declarations for @stdlib/lapack/base/zlaqr0

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Complex multishift QR top-level driver
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
* Complex multishift QR top-level driver
*/
declare var zlaqr0: Routine;

export = zlaqr0;
