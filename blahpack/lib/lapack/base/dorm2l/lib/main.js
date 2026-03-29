
'use strict';

// MODULES //

var dorm2l = require( './dorm2l.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

// NOTE: In stdlib, this uses setReadOnly from @stdlib/utils.

// For standalone development, we use Object.defineProperty directly.
Object.defineProperty( dorm2l, 'ndarray', {
	'value': ndarray,
	'enumerable': false,
	'writable': false,
	'configurable': false
});


// EXPORTS //

module.exports = dorm2l;
