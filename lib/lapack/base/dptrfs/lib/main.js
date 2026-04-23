'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dptrfs = require( './dptrfs.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dptrfs, 'ndarray', ndarray );


// EXPORTS //

module.exports = dptrfs;
