

// TypeScript declarations for @stdlib/lapack/base/dlaqr1

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Sets a scalar multiple of the first column of H - shift product
	*/
	(
		N: number,
		H: Float64Array,
		strideH1: number,
		strideH2: number,
		offsetH: number,
		sr1: number,
		si1: number,
		sr2: number,
		si2: number,
		v: Float64Array,
		strideV: number,
		offsetV: number
	): Float64Array;
}

/**
* Sets a scalar multiple of the first column of H - shift product
*/
declare var dlaqr1: Routine;

export = dlaqr1;
