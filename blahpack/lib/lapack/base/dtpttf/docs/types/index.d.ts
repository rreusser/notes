

// TypeScript declarations for @stdlib/lapack/base/dtpttf

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Copy a triangular matrix from standard packed format (TP) to Rectangular Full Packed format (RFP).
	*/
	(
		transr: string,
		uplo: string,
		N: number,
		AP: Float64Array,
		strideAP: number,
		offsetAP: number,
		ARF: Float64Array,
		strideARF: number,
		offsetARF: number
	): Float64Array;
}

/**
* Copy a triangular matrix from standard packed format (TP) to Rectangular Full Packed format (RFP).
*/
declare var dtpttf: Routine;

export = dtpttf;
