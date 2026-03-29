

// TypeScript declarations for @stdlib/lapack/base/ztrttf

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Copy a triangular matrix from standard full format (TR) to Rectangular Full Packed format (TF)
	*/
	(
		transr: string,
		uplo: string,
		N: number,
		A: Float64Array,
		strideA1: number,
		strideA2: number,
		offsetA: number,
		lda: number,
		ARF: Float64Array,
		strideARF: number,
		offsetARF: number
	): Float64Array;
}

/**
* Copy a triangular matrix from standard full format (TR) to Rectangular Full Packed format (TF)
*/
declare var ztrttf: Routine;

export = ztrttf;
