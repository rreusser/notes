

// TypeScript declarations for @stdlib/lapack/base/dlartg

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Generate a plane rotation (real Givens rotation)
	*/
	(
		f: number,
		g: number,
		c: number,
		s: number,
		r: number
	): void;
}

/**
* Generate a plane rotation (real Givens rotation)
*/
declare var dlartg: Routine;

export = dlartg;
