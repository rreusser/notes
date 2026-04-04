

'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var ztprfs = require( './ztprfs.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( ztprfs, 'ndarray', ndarray );


// EXPORTS //

module.exports = ztprfs;
