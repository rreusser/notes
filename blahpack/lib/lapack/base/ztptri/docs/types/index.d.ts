

// TypeScript declarations for @stdlib/lapack/base/ztptri

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Computes the inverse of a complex triangular matrix in packed storage.
	*/
	(
		uplo: string,
		diag: string,
		N: number,
		AP: Float64Array,
		stride: number,
		offset: number
	): Float64Array;
}

/**
* Computes the inverse of a complex triangular matrix in packed storage.
*/
declare var ztptri: Routine;

export = ztptri;
