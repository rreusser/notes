

// TypeScript declarations for @stdlib/lapack/base/zhptrd

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Reduces a complex Hermitian matrix stored in packed form to real symmetric tridiagonal form.
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
* Reduces a complex Hermitian matrix stored in packed form to real symmetric tridiagonal form.
*/
declare var zhptrd: Routine;

export = zhptrd;
