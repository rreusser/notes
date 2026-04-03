

// TypeScript declarations for @stdlib/lapack/base/zsptrf

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Computes the Bunch-Kaufman factorization of a complex symmetric matrix in packed storage.
	*/
	(
		uplo: string,
		N: number,
		AP: Float64Array,
		strideAP: number,
		offsetAP: number,
		IPIV: Int32Array,
		strideIPIV: number,
		offsetIPIV: number
	): Float64Array;
}

/**
* Computes the Bunch-Kaufman factorization of a complex symmetric matrix in packed storage.
*/
declare var zsptrf: Routine;

export = zsptrf;
