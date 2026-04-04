

// TypeScript declarations for @stdlib/lapack/base/dtptri

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Computes the inverse of a real triangular matrix in packed storage.
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
* Computes the inverse of a real triangular matrix in packed storage.
*/
declare var dtptri: Routine;

export = dtptri;
