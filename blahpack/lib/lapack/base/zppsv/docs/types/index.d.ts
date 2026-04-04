

// TypeScript declarations for @stdlib/lapack/base/zppsv

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Computes the solution to a complex system A * X = B where A is Hermitian positive definite in packed storage.
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
* Computes the solution to a complex system A * X = B where A is Hermitian positive definite in packed storage.
*/
declare var zppsv: Routine;

export = zppsv;
