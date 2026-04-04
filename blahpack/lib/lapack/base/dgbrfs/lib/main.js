
'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dgbrfs = require( './dgbrfs.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dgbrfs, 'ndarray', ndarray );


// EXPORTS //

module.exports = dgbrfs;
