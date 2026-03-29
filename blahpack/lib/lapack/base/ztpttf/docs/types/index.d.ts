

// TypeScript declarations for @stdlib/lapack/base/ztpttf

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Copy a triangular matrix from standard packed format (TP) to rectangular full packed format (RFP), complex version.
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
* Copy a triangular matrix from standard packed format (TP) to rectangular full packed format (RFP), complex version.
*/
declare var ztpttf: Routine;

export = ztpttf;
