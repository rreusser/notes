'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dgerfs = require( './dgerfs.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dgerfs, 'ndarray', ndarray );


// EXPORTS //

module.exports = dgerfs;
