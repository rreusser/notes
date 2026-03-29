

// TypeScript declarations for @stdlib/lapack/base/dtrttf

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Copy a triangular matrix from standard full format (TR) to Rectangular Full Packed format (RFP)
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
* Copy a triangular matrix from standard full format (TR) to Rectangular Full Packed format (RFP)
*/
declare var dtrttf: Routine;

export = dtrttf;
