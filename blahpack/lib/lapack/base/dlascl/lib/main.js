

'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );

var dlascl = require( './dlascl.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dlascl, 'ndarray', ndarray );


// EXPORTS //

module.exports = dlascl;
