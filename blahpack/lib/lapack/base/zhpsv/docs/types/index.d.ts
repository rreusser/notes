

// TypeScript declarations for @stdlib/lapack/base/zhpsv

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Computes the solution to a complex system of linear equations A * X = B where A is Hermitian in packed storage.
	*/
	(
		uplo: string,
		N: number,
		nrhs: number,
		AP: Float64Array,
		strideAP: number,
		offsetAP: number,
		IPIV: Int32Array,
		strideIPIV: number,
		offsetIPIV: number,
		B: Float64Array,
		strideB1: number,
		strideB2: number,
		offsetB: number
	): Float64Array;
}

/**
* Computes the solution to a complex system of linear equations A * X = B where A is Hermitian in packed storage.
*/
declare var zhpsv: Routine;

export = zhpsv;
