

// TypeScript declarations for @stdlib/lapack/base/dpoequ

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Compute row/column scalings for equilibrating a symmetric positive definite matrix
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
* Compute row/column scalings for equilibrating a symmetric positive definite matrix
*/
declare var dpoequ: Routine;

export = dpoequ;
