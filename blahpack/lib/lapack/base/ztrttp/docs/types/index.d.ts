

// TypeScript declarations for @stdlib/lapack/base/ztrttp

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Copy a complex triangular matrix from full format (TR) to standard packed format (TP)
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
* Copy a complex triangular matrix from full format (TR) to standard packed format (TP)
*/
declare var ztrttp: Routine;

export = ztrttp;
