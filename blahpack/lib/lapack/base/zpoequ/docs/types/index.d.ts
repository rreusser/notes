

// TypeScript declarations for @stdlib/lapack/base/zpoequ

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Compute row/column scaling for Hermitian positive definite matrix
	*/
	(
		N: number,
		A: Float64Array,
		strideA1: number,
		strideA2: number,
		offsetA: number,
		s: Float64Array,
		strideS: number,
		offsetS: number,
		scond: number,
		amax: number
	): Float64Array;
}

/**
* Compute row/column scaling for Hermitian positive definite matrix
*/
declare var zpoequ: Routine;

export = zpoequ;
