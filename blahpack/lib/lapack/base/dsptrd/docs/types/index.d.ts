

// TypeScript declarations for @stdlib/lapack/base/dsptrd

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Reduces a real symmetric matrix in packed storage to tridiagonal form.
	*/
	(
		uplo: string,
		N: number,
		AP: Float64Array,
		strideAP: number,
		offsetAP: number,
		d: Float64Array,
		strideD: number,
		offsetD: number,
		e: Float64Array,
		strideE: number,
		offsetE: number,
		TAU: Float64Array,
		strideTAU: number,
		offsetTAU: number
	): Float64Array;
}

/**
* Reduces a real symmetric matrix in packed storage to tridiagonal form.
*/
declare var dsptrd: Routine;

export = dsptrd;
