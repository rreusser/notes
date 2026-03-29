

// TypeScript declarations for @stdlib/lapack/base/ztfttr

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Copy a triangular matrix from Rectangular Full Packed format (RFP) to standard full format (TR)
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
* Copy a triangular matrix from Rectangular Full Packed format (RFP) to standard full format (TR)
*/
declare var ztfttr: Routine;

export = ztfttr;
