

// TypeScript declarations for @stdlib/lapack/base/ztfttp

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Copy a complex triangular matrix from Rectangular Full Packed (RFP) format to standard packed format (TP).
	*/
	(
		transr: string,
		uplo: string,
		N: number,
		ARF: Float64Array,
		strideARF: number,
		offsetARF: number,
		AP: Float64Array,
		strideAP: number,
		offsetAP: number
	): Float64Array;
}

/**
* Copy a complex triangular matrix from Rectangular Full Packed (RFP) format to standard packed format (TP).
*/
declare var ztfttp: Routine;

export = ztfttp;
