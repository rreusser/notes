

// TypeScript declarations for @stdlib/lapack/base/dsyconv

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Converts a symmetric matrix factored by dsytrf to standard L*D*L^T form and vice versa
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
* Converts a symmetric matrix factored by dsytrf to standard L*D*L^T form and vice versa
*/
declare var dsyconv: Routine;

export = dsyconv;
