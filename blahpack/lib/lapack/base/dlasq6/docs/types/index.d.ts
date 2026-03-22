

// TypeScript declarations for @stdlib/lapack/base/dlasq6

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Compute one dqds transform without shift
	*/
	(
		i0: number,
		n0: number,
		z: Float64Array,
		stride: number,
		offset: number,
		pp: number,
		dmin: number,
		dmin1: number,
		dmin2: number,
		dn: number,
		dnm1: number,
		dnm2: number
	): Float64Array;
}

/**
* Compute one dqds transform without shift
*/
declare var dlasq6: Routine;

export = dlasq6;
