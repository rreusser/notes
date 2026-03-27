

// TypeScript declarations for @stdlib/lapack/base/dpbequ

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Compute row and column scalings to equilibrate a symmetric positive definite band matrix.
	*/
	(
		uplo: string,
		N: number,
		kd: number,
		AB: Float64Array,
		strideAB1: number,
		strideAB2: number,
		offsetAB: number,
		s: Float64Array,
		strideS: number,
		offsetS: number,
		scond: number,
		amax: number
	): Float64Array;
}

/**
* Compute row and column scalings to equilibrate a symmetric positive definite band matrix.
*/
declare var dpbequ: Routine;

export = dpbequ;
