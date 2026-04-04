

// TypeScript declarations for @stdlib/lapack/base/zupgtr

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Generates an orthogonal matrix Q which is defined as the product of n-1 elementary reflectors of order n, as returned by zhptrd.
	*/
	(
		uplo: string,
		N: number,
		AP: Float64Array,
		strideAP: number,
		offsetAP: number,
		TAU: Float64Array,
		strideTAU: number,
		offsetTAU: number,
		Q: Float64Array,
		strideQ1: number,
		strideQ2: number,
		offsetQ: number,
		WORK: Float64Array,
		strideWORK: number,
		offsetWORK: number
	): Float64Array;
}

/**
* Generates an orthogonal matrix Q which is defined as the product of n-1 elementary reflectors of order n, as returned by zhptrd.
*/
declare var zupgtr: Routine;

export = zupgtr;
