

// TypeScript declarations for @stdlib/lapack/base/dppsv

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Computes the solution to a real system of linear equations A * X = B where A is symmetric positive definite in packed storage.
	*/
	(
		uplo: string,
		N: number,
		nrhs: number,
		AP: Float64Array,
		strideAP: number,
		offsetAP: number,
		B: Float64Array,
		strideB1: number,
		strideB2: number,
		offsetB: number
	): Float64Array;
}

/**
* Computes the solution to a real system of linear equations A * X = B where A is symmetric positive definite in packed storage.
*/
declare var dppsv: Routine;

export = dppsv;
