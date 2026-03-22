

// TypeScript declarations for @stdlib/lapack/base/dorgbr

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Generate orthogonal matrix Q or P-transpose from a bidiagonal reduction
	*/
	(
		vect: string,
		M: number,
		N: number,
		K: number,
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
* Generate orthogonal matrix Q or P-transpose from a bidiagonal reduction
*/
declare var dorgbr: Routine;

export = dorgbr;
