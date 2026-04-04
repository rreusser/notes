
'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var zgtrfs = require( './zgtrfs.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zgtrfs, 'ndarray', ndarray );


// EXPORTS //

module.exports = zgtrfs;
