

// TypeScript declarations for @stdlib/lapack/base/zsyconv

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Converts a complex symmetric matrix factored by zsytrf to standard form
	*/
	(
		uplo: string,
		way: string,
		N: number,
		A: Float64Array,
		strideA1: number,
		strideA2: number,
		offsetA: number,
		IPIV: Int32Array,
		strideIPIV: number,
		offsetIPIV: number,
		e: Float64Array,
		strideE: number,
		offsetE: number
	): Float64Array;
}

/**
* Converts a complex symmetric matrix factored by zsytrf to standard form
*/
declare var zsyconv: Routine;

export = zsyconv;
