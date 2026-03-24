

// TypeScript declarations for @stdlib/lapack/base/dlaqr4

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Multi-shift QR algorithm for eigenvalues of a Hessenberg matrix
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
		WR: Float64Array,
		strideWR: number,
		offsetWR: number,
		WI: Float64Array,
		strideWI: number,
		offsetWI: number,
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
* Multi-shift QR algorithm for eigenvalues of a Hessenberg matrix
*/
declare var dlaqr4: Routine;

export = dlaqr4;
