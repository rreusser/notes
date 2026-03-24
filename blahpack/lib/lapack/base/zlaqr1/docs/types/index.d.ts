

// TypeScript declarations for @stdlib/lapack/base/zlaqr1

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Set initial vector for Francis QR step
	*/
	(
		N: number,
		H: Float64Array,
		strideH1: number,
		strideH2: number,
		offsetH: number,
		s1: any,
		s2: any,
		v: Float64Array,
		strideV: number,
		offsetV: number
	): Float64Array;
}

/**
* Set initial vector for Francis QR step
*/
declare var zlaqr1: Routine;

export = zlaqr1;
