

// TypeScript declarations for @stdlib/lapack/base/dpftri

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Computes the inverse of a real symmetric positive definite matrix in Rectangular Full Packed format.
	*/
	(
		transr: string,
		uplo: string,
		N: number,
		a: Float64Array,
		stride: number,
		offset: number
	): Float64Array;
}

/**
* Computes the inverse of a real symmetric positive definite matrix in Rectangular Full Packed format.
*/
declare var dpftri: Routine;

export = dpftri;
