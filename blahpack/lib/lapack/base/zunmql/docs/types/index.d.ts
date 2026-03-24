

// TypeScript declarations for @stdlib/lapack/base/zunmql

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Applies a complex unitary matrix Q from a QL factorization to a matrix (blocked)
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
		offsetWORK: number,
		lwork: number
	): Float64Array;
}

/**
* Applies a complex unitary matrix Q from a QL factorization to a matrix (blocked)
*/
declare var zunmql: Routine;

export = zunmql;
