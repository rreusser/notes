

// TypeScript declarations for @stdlib/lapack/base/dsptri

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Computes the inverse of a real symmetric matrix in packed storage using the factorization computed by dsptrf.
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
* Computes the inverse of a real symmetric matrix in packed storage using the factorization computed by dsptrf.
*/
declare var dsptri: Routine;

export = dsptri;
