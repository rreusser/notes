

// TypeScript declarations for @stdlib/lapack/base/dlarra

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Compute the splitting points with threshold based on the representation
	*/
	(
		N: number,
		d: Float64Array,
		strideD: number,
		offsetD: number,
		e: Float64Array,
		strideE: number,
		offsetE: number,
		E2: Float64Array,
		strideE2: number,
		offsetE2: number,
		spltol: number,
		tnrm: number,
		nsplit: number,
		ISPLIT: Int32Array,
		strideISPLIT: number,
		offsetISPLIT: number
	): Float64Array;
}

/**
* Compute the splitting points with threshold based on the representation
*/
declare var dlarra: Routine;

export = dlarra;
