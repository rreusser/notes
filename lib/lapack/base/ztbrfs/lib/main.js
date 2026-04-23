
'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var ztbrfs = require( './ztbrfs.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( ztbrfs, 'ndarray', ndarray );


// EXPORTS //

module.exports = ztbrfs;
