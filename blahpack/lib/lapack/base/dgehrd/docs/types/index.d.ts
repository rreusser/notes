

// TypeScript declarations for @stdlib/lapack/base/dgehrd

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Reduce a general matrix to upper Hessenberg form (blocked)
	*/
	(
		N: number,
		ilo: number,
		ihi: number,
		A: Float64Array,
		strideA1: number,
		strideA2: number,
		offsetA: number,
		TAU: Float64Array,
		strideTAU: number,
		offsetTAU: number,
		WORK: Float64Array,
		strideWORK: number,
		offsetWORK: number,
		lwork: number
	): Float64Array;
}

/**
* Reduce a general matrix to upper Hessenberg form (blocked)
*/
declare var dgehrd: Routine;

export = dgehrd;
