'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dgtrfs = require( './dgtrfs.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dgtrfs, 'ndarray', ndarray );


// EXPORTS //

module.exports = dgtrfs;
