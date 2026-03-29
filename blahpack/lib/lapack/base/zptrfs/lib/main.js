'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var zptrfs = require( './zptrfs.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zptrfs, 'ndarray', ndarray );


// EXPORTS //

module.exports = zptrfs;
