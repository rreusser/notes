

// TypeScript declarations for @stdlib/lapack/base/dtpttr

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Copy a triangular matrix from standard packed format to full format.
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
* Copy a triangular matrix from standard packed format to full format.
*/
declare var dtpttr: Routine;

export = dtpttr;
