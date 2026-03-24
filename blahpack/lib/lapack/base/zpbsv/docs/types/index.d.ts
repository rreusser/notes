

// TypeScript declarations for @stdlib/lapack/base/zpbsv

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Computes the solution to a complex Hermitian positive definite banded system of linear equations A * X = B
	*/
	(
		uplo: string,
		N: number,
		kd: number,
		nrhs: number,
		AB: Float64Array,
		strideAB1: number,
		strideAB2: number,
		offsetAB: number,
		B: Float64Array,
		strideB1: number,
		strideB2: number,
		offsetB: number
	): Float64Array;
}

/**
* Computes the solution to a complex Hermitian positive definite banded system of linear equations A * X = B
*/
declare var zpbsv: Routine;

export = zpbsv;
