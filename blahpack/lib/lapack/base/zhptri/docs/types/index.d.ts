

// TypeScript declarations for @stdlib/lapack/base/zhptri

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Computes the inverse of a complex Hermitian matrix in packed storage using the factorization computed by zhptrf.
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
* Computes the inverse of a complex Hermitian matrix in packed storage using the factorization computed by zhptrf.
*/
declare var zhptri: Routine;

export = zhptri;
