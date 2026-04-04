
'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var zhprfs = require( './zhprfs.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zhprfs, 'ndarray', ndarray );


// EXPORTS //

module.exports = zhprfs;
