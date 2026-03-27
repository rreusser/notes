

// TypeScript declarations for @stdlib/lapack/base/dtrttp

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Copy a triangular matrix from full format to standard packed format.
	*/
	(
		uplo: string,
		N: number,
		A: Float64Array,
		strideA1: number,
		strideA2: number,
		offsetA: number,
		AP: Float64Array,
		strideAP: number,
		offsetAP: number
	): Float64Array;
}

/**
* Copy a triangular matrix from full format to standard packed format.
*/
declare var dtrttp: Routine;

export = dtrttp;
