

// TypeScript declarations for @stdlib/lapack/base/dtfttr

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Copy a triangular matrix from Rectangular Full Packed format to standard full format
	*/
	(
		transr: string,
		uplo: string,
		N: number,
		ARF: Float64Array,
		strideARF: number,
		offsetARF: number,
		A: Float64Array,
		strideA1: number,
		strideA2: number,
		offsetA: number,
		lda: number
	): Float64Array;
}

/**
* Copy a triangular matrix from Rectangular Full Packed format to standard full format
*/
declare var dtfttr: Routine;

export = dtfttr;
