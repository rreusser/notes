

// TypeScript declarations for @stdlib/lapack/base/zunm2r

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Apply orthogonal matrix Q from QR factorization to a matrix (unblocked)
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
* Apply orthogonal matrix Q from QR factorization to a matrix (unblocked)
*/
declare var zunm2r: Routine;

export = zunm2r;
