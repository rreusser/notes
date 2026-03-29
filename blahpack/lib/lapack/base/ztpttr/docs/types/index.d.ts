

// TypeScript declarations for @stdlib/lapack/base/ztpttr

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Copies a complex triangular matrix from packed storage to full storage
	*/
	(
		uplo: string,
		N: number,
		AP: Float64Array,
		strideAP: number,
		offsetAP: number,
		A: Float64Array,
		strideA1: number,
		strideA2: number,
		offsetA: number
	): Float64Array;
}

/**
* Copies a complex triangular matrix from packed storage to full storage
*/
declare var ztpttr: Routine;

export = ztpttr;
