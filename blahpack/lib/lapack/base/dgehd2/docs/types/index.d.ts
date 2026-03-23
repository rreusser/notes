

// TypeScript declarations for @stdlib/lapack/base/dgehd2

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Reduce a general matrix to upper Hessenberg form (unblocked)
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
		offsetWORK: number
	): Float64Array;
}

/**
* Reduce a general matrix to upper Hessenberg form (unblocked)
*/
declare var dgehd2: Routine;

export = dgehd2;
