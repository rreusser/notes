
'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var zgbrfs = require( './zgbrfs.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zgbrfs, 'ndarray', ndarray );


// EXPORTS //

module.exports = zgbrfs;
