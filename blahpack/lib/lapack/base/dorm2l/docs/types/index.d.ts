

// TypeScript declarations for @stdlib/lapack/base/dorm2l

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Multiply a matrix by the orthogonal matrix Q from QL factorization (unblocked)
	*/
	(
		side: string,
		trans: string,
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
		C: Float64Array,
		strideC1: number,
		strideC2: number,
		offsetC: number,
		WORK: Float64Array,
		strideWORK: number,
		offsetWORK: number
	): Float64Array;
}

/**
* Multiply a matrix by the orthogonal matrix Q from QL factorization (unblocked)
*/
declare var dorm2l: Routine;

export = dorm2l;
