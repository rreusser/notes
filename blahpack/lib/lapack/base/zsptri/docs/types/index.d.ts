

// TypeScript declarations for @stdlib/lapack/base/zsptri

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Computes the inverse of a complex symmetric matrix in packed storage using the factorization computed by zsptrf.
	*/
	(
		uplo: string,
		N: number,
		AP: Float64Array,
		strideAP: number,
		offsetAP: number,
		IPIV: Int32Array,
		strideIPIV: number,
		offsetIPIV: number,
		WORK: Float64Array,
		strideWORK: number,
		offsetWORK: number
	): Float64Array;
}

/**
* Computes the inverse of a complex symmetric matrix in packed storage using the factorization computed by zsptrf.
*/
declare var zsptri: Routine;

export = zsptri;
