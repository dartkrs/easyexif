/**************************************************************************
  exif.cpp  -- A simple ISO C++ library to parse basic EXIF 
               information from a JPEG file.

  Copyright (c) 2010-2015 Mayank Lahiri
  mlahiri@gmail.com
  All rights reserved (BSD License).

  See exif.h for version history.

  Redistribution and use in source and binary forms, with or without 
  modification, are permitted provided that the following conditions are met:

  -- Redistributions of source code must retain the above copyright notice, 
     this list of conditions and the following disclaimer.
  -- Redistributions in binary form must reproduce the above copyright notice, 
     this list of conditions and the following disclaimer in the documentation 
     and/or other materials provided with the distribution.

     THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS ``AS IS'' AND ANY EXPRESS 
     OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES 
     OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN 
     NO EVENT SHALL THE FREEBSD PROJECT OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, 
     INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, 
     BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, 
     DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY 
     OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING 
     NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, 
     EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/
#include "exif.h"
#include "fmountlens4.h"

#include <algorithm>
#include <cstdint>
#include <stdio.h>
#include <vector>

using std::string;

namespace {

  struct Rational {
    uint32_t numerator, denominator;
    operator double() const {
      if (denominator < 1e-20) {
        return 0;
      }
      return static_cast<double>(numerator) / static_cast<double>(denominator);
    }
  };

  // IF Entry 
  class IFEntry {
  public:
    using byte_vector = std::vector<uint8_t>;
    using ascii_vector = std::string;
    using short_vector = std::vector<uint16_t>;
    using long_vector = std::vector<uint32_t>;
    using rational_vector = std::vector<Rational>;

    IFEntry()
      : tag_(0xFF),
        format_(0xFF),
        data_(0),
        length_(0),
        val_byte_(nullptr)
    {}
    IFEntry(unsigned short ntag, unsigned short nformat, unsigned nlength, unsigned ndata) {
      tag(ntag);
      format(nformat);
      length(nlength);
      data(ndata);
    }
    IFEntry(const IFEntry&) =delete;
    IFEntry & operator=(const IFEntry &) =delete;
    IFEntry(IFEntry && other)
      : tag_(other.tag_),
        format_(other.format_),
        data_(other.data_),
        length_(other.length_),
        val_byte_(other.val_byte_)
    {
      other.tag_ = 0xFF;
      other.format_ = 0xFF;
      other.data_ = 0;
      other.length_ = 0;
      other.val_byte_ = nullptr;
    }
    ~IFEntry() {
      delete_union();
    }
    unsigned short tag() const {
      return tag_;
    }
    void tag(unsigned short tag) {
      tag_ = tag;
    }
    unsigned short format() const {
      return format_;
    }
    bool format(unsigned short format) {
      switch (format) {
        case 0x01:
        case 0x02:
        case 0x03:
        case 0x04:
        case 0x05:
        case 0x07:
        case 0x09:
        case 0x0a:
        case 0xff:
          break;
        default:
          return false;
      }
      delete_union();
      format_ = format;
      new_union();
      return true;
    }
    unsigned data() const {
      return data_;
    }
    void data(unsigned data) {
      data_ = data;
    }
    unsigned length() const {
      return length_;
    }
    void length(unsigned length) {
      length_ = length;
    }

    // functions to access the data
    //
    // !! it's CALLER responsibility to check that format !!
    // !! is correct before accessing it's field          !!
    //
    // - getters are use here to allow future addition
    //   of checks if format is correct
    byte_vector & val_byte() {
      return *val_byte_;
    }
    ascii_vector & val_string() {
      return *val_string_;
    }
    short_vector & val_short() {
      return *val_short_;
    }
    long_vector & val_long() {
      return *val_long_;
    }
    rational_vector & val_rational() {
      return *val_rational_;
    }
  private:
    // Raw fields
    unsigned short tag_;
    unsigned short format_;
    unsigned data_;
    unsigned length_;
    
    // Parsed fields
    union {
      byte_vector * val_byte_;
      ascii_vector * val_string_;
      short_vector * val_short_;
      long_vector * val_long_;
      rational_vector * val_rational_;
    };

    void delete_union() {
      switch (format_) {
        case 0x1:
        case 0x7:
          delete val_byte_;
          val_byte_ = nullptr;
          break;
        case 0x2:
          delete val_string_;
          val_string_ = nullptr;
          break;
        case 0x3:
          delete val_short_;
          val_short_ = nullptr;
          break;
        case 0x4:
          delete val_long_;
          val_long_ = nullptr;
          break;
        case 0x5:
          delete val_rational_;
          val_rational_ = nullptr;
          break;
        case 0xff:
          break;
        default:
          // should not get here
          // should I throw an exception or ...?
          break;
      }
    }
    void new_union() {
      switch (format_) {
        case 0x1:
        case 0x7:
          val_byte_ = new byte_vector();
          break;
        case 0x2:
          val_string_ = new ascii_vector();
          break;
        case 0x3:
          val_short_ = new short_vector();
          break;
        case 0x4:
          val_long_ = new long_vector();
          break;
        case 0x5:
          val_rational_ = new rational_vector();
          break;
        case 0xff:
          break;
        default:
          // should not get here
          // should I throw an exception or ...?
          break;
      }
    }
  };

  // Helper functions
  template <typename T, bool alignIntel>
  T parse(const unsigned char * buf);

  template <>
  uint8_t parse<uint8_t, false>(const unsigned char * buf) {
    return *buf;
  }

  template <>
  uint8_t parse<uint8_t, true>(const unsigned char * buf) {
    return *buf;
  }

  template <>
  uint16_t parse<uint16_t, false>(const unsigned char * buf) {
    return (static_cast<uint16_t>(buf[0])<<8) | buf[1];
  }

  template <>
  uint16_t parse<uint16_t, true>(const unsigned char * buf) {
    return (static_cast<uint16_t>(buf[1])<<8) | buf[0];
  }

  template <>
  int16_t parse<int16_t, false>(const unsigned char * buf)
  {
	  return (static_cast<int16_t>(buf[0]) << 8) | buf[1];
  }

  template <>
  int16_t parse<int16_t, true>(const unsigned char * buf)
  {
	  return (static_cast<int16_t>(buf[1]) << 8) | buf[0];
  }

  template <>
  uint32_t parse<uint32_t, false>(const unsigned char * buf) {
    return (static_cast<uint32_t>(buf[0])<<24) |
           (static_cast<uint32_t>(buf[1])<<16) |
           (static_cast<uint32_t>(buf[2])<<8)  |
           buf[3];
  }

  template <>
  uint32_t parse<uint32_t, true>(const unsigned char * buf) {
    return (static_cast<uint32_t>(buf[3])<<24) |
           (static_cast<uint32_t>(buf[2])<<16) |
           (static_cast<uint32_t>(buf[1])<<8)  |
           buf[0];
  }

  template <>
  Rational parse<Rational, true>(const unsigned char * buf) {
    Rational r;
    r.numerator   = parse<uint32_t, true>(buf);
    r.denominator = parse<uint32_t, true>(buf + 4);
    return r;
  }

  template <>
  Rational parse<Rational, false>(const unsigned char * buf) {
    Rational r;
    r.numerator   = parse<uint32_t, false>(buf);
    r.denominator = parse<uint32_t, false>(buf + 4);
    return r;
  }

  // This function should clean up container if it's std::string (meaning
  // that it should trim it after first \0.
  //
  template <typename C>
  void normalize_container(C &) {
  }

  template <>
  void normalize_container<std::string>(std::string & container) {
    container = container.c_str();
  }

  /**
   * Try to read entry.length() values for this entry.
   *
   * Returns:
   *  true  - entry.length() values were read
   *  false - something went wrong, vec's content was not touched
   */
  template <typename T, bool alignIntel, typename C>
  bool extract_values(C & container,
                      const unsigned char * buf,
                      const unsigned base,
                      const unsigned len,
                      const IFEntry & entry) {
    const unsigned char * data;
    uint32_t reversed_data;
    // if data fits into 4 bytes, they are stored directly in
    // the data field in IFEntry
    if (sizeof(T) * entry.length() <= 4) {
      if (alignIntel) {
        reversed_data = entry.data();
      } else {
        reversed_data = entry.data();
        // this reversing works, but is ugly 
        unsigned char * data = reinterpret_cast<unsigned char *>(&reversed_data);
        unsigned char tmp;
        tmp = data[0];
        data[0] = data[3];
        data[3] = tmp;
        tmp = data[1];
        data[1] = data[2];
        data[2] = tmp;
      }
      data = reinterpret_cast<const unsigned char *>(&(reversed_data));
    } else {
      data = buf + base + entry.data();
      if (data + sizeof(T) * entry.length() > buf + len) {
        return false;
      }
    }
    container.resize(entry.length());
    for (size_t i = 0; i < entry.length(); ++i) {
      container[i] = parse<T, alignIntel>(data + sizeof(T) * i);
    }
    // Needed because there can sometimes be trash after \0 in ASCII string
    // probably left for alignment reasons. Pretty sure it's supposed to be
    // ignored.
    normalize_container(container);
    return true;
  }

  template <bool alignIntel>
  void parseIFEntryHeader(const unsigned char * buf,
                          unsigned short & tag,
                          unsigned short & format,
                          unsigned & length,
                          unsigned & data) {
    // Each directory entry is composed of:
    // 2 bytes: tag number (data field)
    // 2 bytes: data format
    // 4 bytes: number of components
    // 4 bytes: data value or offset to data value
    tag    = parse<uint16_t, alignIntel>(buf);
    format = parse<uint16_t, alignIntel>(buf + 2);
    length = parse<uint32_t, alignIntel>(buf + 4);
    data   = parse<uint32_t, alignIntel>(buf + 8);
  }

  template <bool alignIntel>
  void parseIFEntryHeader(const unsigned char * buf,
                          IFEntry & result) {
    unsigned short tag;
    unsigned short format;
    unsigned length;
    unsigned data;

    parseIFEntryHeader<alignIntel>(buf, tag, format, length, data);

    result.tag(tag);
    result.format(format);
    result.length(length);
    result.data(data);
  }

  template <bool alignIntel>
  IFEntry parseIFEntry_temp(const unsigned char * buf,
                       const unsigned offs,
                       const unsigned base,
                       const unsigned len) {
    IFEntry result;

    // check if there even is enough data for IFEntry in the buffer
    if (buf + offs + 12 > buf + len) {
      result.tag(0xFF);
      return result;
    }

    parseIFEntryHeader<alignIntel>(buf + offs, result);

    // Parse value in specified format
    switch (result.format()) {
      case 1:
      case 7:
        if (!extract_values<uint8_t, alignIntel>(result.val_byte(), buf, base, len, result)) {
          result.tag(0xFF);
        }
        break;
      case 2:
        // string is basically sequence of uint8_t (well, according to EXIF even uint7_t, but
        // we don't have that), so just read it as bytes
        if (!extract_values<uint8_t, alignIntel>(result.val_string(), buf, base, len, result)) {
          result.tag(0xFF);
        }
        // and cut zero byte at the end, since we don't want that in the std::string
        if (result.val_string().length() && result.val_string()[result.val_string().length() - 1] == '\0') {
          result.val_string().resize(result.val_string().length() - 1);
        }
        break;
      case 3:
        if (!extract_values<uint16_t, alignIntel>(result.val_short(), buf, base, len, result)) {
          result.tag(0xFF);
        }
        break;
      case 4:
        if (!extract_values<uint32_t, alignIntel>(result.val_long(), buf, base, len, result)) {
          result.tag(0xFF);
        }
        break;
      case 5:
        if (!extract_values<Rational, alignIntel>(result.val_rational(), buf, base, len, result)) {
          result.tag(0xFF);
        }
        break;
      case 9:
      case 10:
        break;
      default:
        result.tag(0xFF);
    }
    return result;
  }

  // helper functions for convinience
  template <typename T>
  T parse_value(const unsigned char * buf, bool alignIntel) {
    if (alignIntel) {
      return parse<T, true>(buf);
    } else {
      return parse<T, false>(buf);
    }
  }

  void parseIFEntryHeader(const unsigned char * buf,
                          bool alignIntel,
                          unsigned short & tag,
                          unsigned short & format,
                          unsigned & length,
                          unsigned & data) {
    if (alignIntel) {
      parseIFEntryHeader<true>(buf, tag, format, length, data);
    } else {
      parseIFEntryHeader<false>(buf, tag, format, length, data);
    }
  }

  IFEntry parseIFEntry(const unsigned char * buf,
                       const unsigned offs,
                       const bool alignIntel,
                       const unsigned base,
                       const unsigned len) {
    if (alignIntel) {
      return std::move(parseIFEntry_temp<true>(buf, offs, base, len));
    } else {
      return std::move(parseIFEntry_temp<false>(buf, offs, base, len));
    }
  }
}

//
// Locates the EXIF segment and parses it using parseFromEXIFSegment 
//
int EXIFInfo::parseFrom(const unsigned char *buf, unsigned len) {
  // Sanity check: all JPEG files start with 0xFFD8 and end with 0xFFD9
  // This check also ensures that the user has supplied a correct value for len.
  if (!buf || len < 4)
    return PARSE_EXIF_ERROR_NO_EXIF;
  if (buf[0] != 0xFF || buf[1] != 0xD8)
    return PARSE_EXIF_ERROR_NO_JPEG;
  if (buf[len-2] != 0xFF || buf[len-1] != 0xD9)
    return PARSE_EXIF_ERROR_NO_JPEG;
  clear();

  // Scan for EXIF header (bytes 0xFF 0xE1) and do a sanity check by 
  // looking for bytes "Exif\0\0". The marker length data is in Motorola
  // byte order, which results in the 'false' parameter to parse16().
  // The marker has to contain at least the TIFF header, otherwise the
  // EXIF data is corrupt. So the minimum length specified here has to be:
  //   2 bytes: section size
  //   6 bytes: "Exif\0\0" string
  //   2 bytes: TIFF header (either "II" or "MM" string)
  //   2 bytes: TIFF magic (short 0x2a00 in Motorola byte order)
  //   4 bytes: Offset to first IFD
  // =========
  //  16 bytes
  unsigned offs = 0;        // current offset into buffer
  for (offs = 0; offs < len-1; offs++) 
    if (buf[offs] == 0xFF && buf[offs+1] == 0xE1) 
      break;
  if (offs + 4 > len)
    return PARSE_EXIF_ERROR_NO_EXIF;
  offs += 2;
  unsigned short section_length = parse_value<uint16_t>(buf + offs, false);
  if (offs + section_length > len || section_length < 16)
    return PARSE_EXIF_ERROR_CORRUPT;
  offs += 2;

  return parseFromEXIFSegment(buf + offs, len - offs);
}

int EXIFInfo::parseFrom(const string &data) {
  return parseFrom((const unsigned char *)data.data(), data.length());
}

//
// Main parsing function for an EXIF segment.
//
// PARAM: 'buf' start of the EXIF TIFF, which must be the bytes "Exif\0\0".
// PARAM: 'len' length of buffer
//
int EXIFInfo::parseFromEXIFSegment(const unsigned char *buf, unsigned len)
{
	bool alignIntel = true;     // byte alignment (defined in EXIF header)
	unsigned offs = 0;        // current offset into buffer
	if (!buf || len < 6)
		return PARSE_EXIF_ERROR_NO_EXIF;

	if (!std::equal(buf, buf + 6, "Exif\0\0"))
		return PARSE_EXIF_ERROR_NO_EXIF;
	offs += 6;

	// Now parsing the TIFF header. The first two bytes are either "II" or
	// "MM" for Intel or Motorola byte alignment. Sanity check by parsing
	// the unsigned short that follows, making sure it equals 0x2a. The
	// last 4 bytes are an offset into the first IFD, which are added to 
	// the global offset counter. For this block, we expect the following
	// minimum size:
	//  2 bytes: 'II' or 'MM'
	//  2 bytes: 0x002a
	//  4 bytes: offset to first IDF
	// -----------------------------
	//  8 bytes
	if (offs + 8 > len)
		return PARSE_EXIF_ERROR_CORRUPT;
	unsigned tiff_header_start = offs;
	if (buf[offs] == 'I' && buf[offs + 1] == 'I')
		alignIntel = true;
	else
	{
		if (buf[offs] == 'M' && buf[offs + 1] == 'M')
			alignIntel = false;
		else
			return PARSE_EXIF_ERROR_UNKNOWN_BYTEALIGN;
	}
	this->ByteAlign = alignIntel;
	offs += 2;
	if (0x2a != parse_value<uint16_t>(buf + offs, alignIntel))
		return PARSE_EXIF_ERROR_CORRUPT;
	offs += 2;
	unsigned first_ifd_offset = parse_value<uint32_t>(buf + offs, alignIntel);
	offs += first_ifd_offset - 4;
	if (offs >= len)
		return PARSE_EXIF_ERROR_CORRUPT;

	// Now parsing the first Image File Directory (IFD0, for the main image).
	// An IFD consists of a variable number of 12-byte directory entries. The
	// first two bytes of the IFD section contain the number of directory
	// entries in the section. The last 4 bytes of the IFD contain an offset
	// to the next IFD, which means this IFD must contain exactly 6 + 12 * num
	// bytes of data.
	if (offs + 2 > len)
		return PARSE_EXIF_ERROR_CORRUPT;
	int num_entries = parse_value<uint16_t>(buf + offs, alignIntel);
	if (offs + 6 + 12 * num_entries > len)
		return PARSE_EXIF_ERROR_CORRUPT;
	offs += 2;
	unsigned exif_sub_ifd_offset = len;
	unsigned gps_sub_ifd_offset = len;
	std::vector<unsigned char> maker_note;

	while (--num_entries >= 0)
	{
		IFEntry result = parseIFEntry(buf, offs, alignIntel, tiff_header_start, len);	// 9A82  0500    0100   0000  4A02  0000
		offs += 12;																		// tag   format  length       value    

		switch (result.tag())
		{
		case 0x102:
			// Bits per sample
			if (result.format() == 3)
				this->BitsPerSample = result.val_short().front();
			break;

		case 0x10E:
			// Image description
			if (result.format() == 2)
				this->ImageDescription = result.val_string();
			break;

		case 0x10F:
			// Digicam make
			if (result.format() == 2)
				this->Make = result.val_string();
			break;

		case 0x110:
			// Digicam model
			if (result.format() == 2)
				this->Model = result.val_string();
			break;

		case 0x112:
			// Orientation of image
			if (result.format() == 3)
				this->Orientation = result.val_short().front();
			break;

		case 0x131:
			// Software used for image
			if (result.format() == 2)
				this->Software = result.val_string();
			break;

		case 0x132:
			// EXIF/TIFF date/time of image modification
			if (result.format() == 2)
				this->DateTime = result.val_string();
			break;

		case 0x8298:
			// Copyright information
			if (result.format() == 2)
				this->Copyright = result.val_string();
			break;

		case 0x8825:
			// GPS IFS offset
			gps_sub_ifd_offset = tiff_header_start + result.data();
			break;

		case 0x8769:
			// EXIF SubIFD offset
			exif_sub_ifd_offset = tiff_header_start + result.data();
			break;
		}
	}

	// Jump to the EXIF SubIFD if it exists and parse all the information
	// there. Note that it's possible that the EXIF SubIFD doesn't exist.
	// The EXIF SubIFD contains most of the interesting information that a
	// typical user might want.
	if (exif_sub_ifd_offset + 4 <= len)
	{
		offs = exif_sub_ifd_offset;
		int num_entries = parse_value<uint16_t>(buf + offs, alignIntel);
		if (offs + 6 + 12 * num_entries > len)
			return PARSE_EXIF_ERROR_CORRUPT;

		offs += 2;
		while (--num_entries >= 0)
		{
			IFEntry result = parseIFEntry(buf, offs, alignIntel, tiff_header_start, len);		
			offs += 12;

			switch (result.tag())
			{
			case 0x829a:
				// Exposure time in seconds
				if (result.format() == 5)
					this->ExposureTime = result.val_rational().front();
				break;

			case 0x829d:
				// FNumber
				if (result.format() == 5)
					this->FNumber = result.val_rational().front();
				break;

			case 0x8827:
				// ISO Speed Rating
				if (result.format() == 3)
					this->ISOSpeedRatings = result.val_short().front();
				break;

			case 0x9003:
				// Original date and time
				if (result.format() == 2)
					this->DateTimeOriginal = result.val_string();
				break;

			case 0x9004:
				// Digitization date and time
				if (result.format() == 2)
					this->DateTimeDigitized = result.val_string();
				break;

			case 0x9201:
				// Shutter speed value
				if (result.format() == 5)
					this->ShutterSpeedValue = result.val_rational().front();
				break;

			case 0x9204:
				// Exposure bias value 
				if (result.format() == 5)
					this->ExposureBiasValue = result.val_rational().front();
				break;

			case 0x9206:
				// Subject distance
				if (result.format() == 5)
					this->SubjectDistance = result.val_rational().front();
				break;

			case 0x9209:
				// Flash used
				if (result.format() == 3)
					this->Flash = result.data() ? 1 : 0;
				break;

			case 0x920a:
				// Focal length
				if (result.format() == 5)
					this->FocalLength = result.val_rational().front();
				break;

			case 0x9207:
				// Metering mode
				if (result.format() == 3)
					this->MeteringMode = result.val_short().front();
				break;

			case 0x927c:
				if (result.format() == 7)
				{
					maker_note.swap(result.val_byte());
				}
				break;

			case 0x9286:
				if (result.format() == 2)
					this->Comment = result.val_string();
				break;

			case 0x9291:
				// Subsecond original time
				if (result.format() == 2)
					this->SubSecTimeOriginal = result.val_string();
				break;

			case 0xa002:
				// EXIF Image width
				if (result.format() == 4)
					this->ImageWidth = result.val_long().front();
				if (result.format() == 3)
					this->ImageWidth = result.val_short().front();
				break;

			case 0xa003:
				// EXIF Image height
				if (result.format() == 4)
					this->ImageHeight = result.val_long().front();
				if (result.format() == 3)
					this->ImageHeight = result.val_short().front();
				break;

			case 0xa405:
				// Focal length in 35mm film
				if (result.format() == 3)
					this->FocalLengthIn35mm = result.val_short().front();
				break;
			case 0xa431:
				if (result.format() == 2)
					this->SerialNumber = result.val_string();
				break;
			case 0xa432:
				if (result.format() == 5)
				{
					this->LensInfo.FocalLengthMin = result.val_rational()[0];
					this->LensInfo.FocalLengthMax = result.val_rational()[1];
					this->LensInfo.FStopMin = result.val_rational()[2];
					this->LensInfo.FStopMax = result.val_rational()[3];
				}
				break;
			case 0xa433:
				if (result.format() == 2)
				{
					this->LensInfo.Make = result.val_string();
				}
				break;
			case 0xa434:
				if (result.format() == 2)
				{
					this->LensInfo.Model = result.val_string();
				}
				break;
			case 0xa435:
				if (result.format() == 2)
					this->LensInfo.SerialNumber = result.val_string();
				break;
			}
		}
	}

	if (!maker_note.empty())
	{
		std::string make;
		make.resize(this->Make.size());
		std::transform(begin(this->Make), end(this->Make), begin(make), ::tolower);
		// let's make tests based on make.. not failproof, but good enough (for now?)
		if (make == "canon")
		{
			// canon format
			const unsigned char * make_buf = maker_note.data();
			int num_entries = parse_value<uint16_t>(make_buf, alignIntel);
			size_t off = 2;

			while (--num_entries >= 0)
			{
				if (off + 12 > maker_note.size())
				{
					break;
				}
				unsigned short tag, format;
				unsigned int length, data;
				parseIFEntryHeader(make_buf + off, alignIntel, tag, format, length, data);
				IFEntry entry(tag, format, length, data);

				switch (tag)
				{
				case 0x0001:
					if (format == 3)
					{
						// camera settings jsou na adrese entry.val_short().front()
						// je to tabulka o delce length, kde pozice v tabulce urcuje klic
						// http://www.exiv2.org/tags-canon.html
						// http://www.ozhiker.com/electronics/pjmt/jpeg_info/canon_mn.html#Camera_Settings_1

						/*
						for (int cstag = 0; cstag < length / 2; cstag++)
						{
							switch (cstag)
							{
								case 22:
								{
									int16_t lenstype = parse_value<int16_t>(buf + entry.data() + cstag * 2, alignIntel);
									break;
								}

								...
							}
						}
						*/
					}
					break;

				case 0x0095:
					if (format == 2)
					{						
						if (extract_values<uint8_t, true>(entry.val_string(), buf, tiff_header_start, len, entry))
						{
							this->LensInfo.FromMakerNote = entry.val_string();
						}
					}
					break;
				}

				off += 12;
			}
		}
		else if (make == "nikon corporation" || make == "nikon")
		{
			const unsigned char * maker_buf = maker_note.data();
			// some of nikon formats
			const char * nikon0 = "Nikon\0";
			if (maker_note.size() >= 6 && std::equal(nikon0, nikon0 + 6, begin(maker_note)))
			{
				// nikon format 2 or 3
				if (maker_note.size() >= 12 && maker_note[10] == maker_note[11] && (maker_note[10] == 'I' || maker_note[11] == 'M'))
				{
					// nikon format 3
					size_t off = 10;
					if (off + 12 > maker_note.size())
					{
						goto end_of_maker_note;
					}
					// here would be nice to refactor actuall TIFF & IFD parsing code
					// into separate functions, but I'm still waiting for upstream to
					// merge my last PR, so don't want to really do that
					// TL;DR: following code will not exactly follow DRY
					bool alignIntel;
					switch (maker_note[off])
					{
					case 'I':
						alignIntel = true;
						break;
					case 'M':
						alignIntel = false;
						break;
					default:
						goto end_of_maker_note;
					}
					off += 2;
					if (0x2a != parse_value<uint16_t>(maker_buf + off, alignIntel))
					{
						goto end_of_maker_note;
					}
					off += 2;
					unsigned int first_ifd_offset = parse_value<uint32_t>(maker_buf + off, alignIntel);
					off += first_ifd_offset - 4;
					if (off + 2 > maker_note.size())
					{
						goto end_of_maker_note;
					}
					int num_entries = parse_value<uint16_t>(maker_buf + off, alignIntel);
					if (off + 6 + 16 * num_entries > maker_note.size())
					{
						goto end_of_maker_note;
					}
					off += 2;

					uint8_t lenstype = 0;
					uint32_t shutterCount = 0;
					std::vector<uint8_t> lensinfo;

					while (--num_entries >= 0)
					{
						IFEntry result = parseIFEntry(maker_buf, off, alignIntel, 10, maker_note.size());
						off += 12;
						
						if (result.tag() == 0x001d)
						{
							if (result.format() == 2)
							{
								this->SerialNumber = result.val_string();
							}
						}

						else if (result.tag() == 0x00a7)
						{
							shutterCount = result.val_long().front();
						}

						else if (result.tag() == 0x0083)
						{
							if (result.format() == 1)
							{
								lenstype = result.val_byte()[0];
							}
						}		

						else if (result.tag() == 0x0084)
						{
							if (result.format() == 5 && result.length() == 4)
							{
								this->LensInfo.FocalLengthMin = result.val_rational()[0];
								this->LensInfo.FocalLengthMax = result.val_rational()[1];
								this->LensInfo.FStopMin = result.val_rational()[2];
								this->LensInfo.FStopMax = result.val_rational()[3];
							}
						}

						else if (result.tag() == 0x0098)
						{
							if (result.val_byte().size() < 4)
							{
								goto end_of_maker_note;
							}

							lensinfo = result.val_byte();
						}
					}



					if (!lensinfo.empty())
					{
						uint8_t lensId = 0;
						uint8_t fstop = 0;
						uint8_t minfocal = 0;
						uint8_t maxfocal = 0;
						uint8_t maxaperturemin = 0;
						uint8_t maxaperturemax = 0;
						uint8_t mcuversion = 0;


						// handling different versions of maker note
						// http://www.sno.phy.queensu.ca/~phil/exiftool/TagNames/Nikon.html#LensData00

						// 0100
						if (lensinfo[0] == '0' && lensinfo[1] == '1' && lensinfo[2] == '0' && lensinfo[3] == '0')
						{
							if (lensinfo.size() < 13)
							{
								goto end_of_maker_note;
							}
							lensId = lensinfo[6];
							fstop = lensinfo[7];
							minfocal = lensinfo[8];
							maxfocal = lensinfo[9];
							maxaperturemin = lensinfo[10];
							maxaperturemax = lensinfo[11];
							mcuversion = lensinfo[12];
						}

						// 0101
						else if (lensinfo[0] == '0' && lensinfo[1] == '1' && lensinfo[2] == '0' && lensinfo[3] == '1')
						{
							if (lensinfo.size() < 19)
							{
								goto end_of_maker_note;
							}
							lensId = lensinfo[11];
							fstop = lensinfo[12];
							minfocal = lensinfo[13];
							maxfocal = lensinfo[14];
							maxaperturemin = lensinfo[15];
							maxaperturemax = lensinfo[16];
							mcuversion = lensinfo[17];
						}

						// 020(1-3) - encrypted
						else if (lensinfo[0] == '0' && lensinfo[1] == '2' && lensinfo[2] == '0' && (lensinfo[3] == '1' || lensinfo[3] == '2' || lensinfo[3] == '3'))
						{
							if (lensinfo.size() < 4)
							{
								goto end_of_maker_note;
							}

							uint32_t serial = atoi(this->SerialNumber.c_str());
							nikon_decrypt(lensinfo.data() + 4, lensinfo.size() - 4, shutterCount, serial);

							lensId = lensinfo[11];
							fstop = lensinfo[12];
							minfocal = lensinfo[13];
							maxfocal = lensinfo[14];
							maxaperturemin = lensinfo[15];
							maxaperturemax = lensinfo[16];
							mcuversion = lensinfo[17];
						}

						// 0204 - encrypted
						else if (lensinfo[0] == '0' && lensinfo[1] == '2' && lensinfo[2] == '0' && lensinfo[3] == '4')
						{
							if (lensinfo.size() < 4)
							{
								goto end_of_maker_note;
							}

							uint32_t serial = atoi(this->SerialNumber.c_str());
							nikon_decrypt(lensinfo.data() + 4, lensinfo.size() - 4, shutterCount, serial);

							lensId = lensinfo[12];
							fstop = lensinfo[13];
							minfocal = lensinfo[14];
							maxfocal = lensinfo[15];
							maxaperturemin = lensinfo[16];
							maxaperturemax = lensinfo[17];
							mcuversion = lensinfo[18];
						}

						// 0400 - encrypted
						else if (lensinfo[0] == '0' && lensinfo[1] == '4' && lensinfo[2] == '0' && lensinfo[3] == '0')
						{
							//TODO - vyzkouset

							if (lensinfo.size() >= 4)
							{
								uint32_t serial = atoi(this->SerialNumber.c_str());
								nikon_decrypt(lensinfo.data() + 4, lensinfo.size() - 4, shutterCount, serial);

								this->LensInfo.FromMakerNote = lensinfo[394];								
							}

							goto end_of_maker_note;
						}

						else
						{
							// unsupported version
							goto end_of_maker_note;
						}



						std::vector<string> possible_lenses;
						for (auto r : fmountlens)
						{
							if (!r.manuf && !r.lensname && !r.lnumber)
								break;

							if (r.lid == lensId && r.stps == fstop && r.focs == minfocal && r.focl == maxfocal && r.aps == maxaperturemin && r.apl == maxaperturemax)
							{
								string name = string(r.manuf).append(" ").append(r.lensname);

								bool found = false;
								for (unsigned a = 0; a < possible_lenses.size(); a++)
								{
									if (possible_lenses[a] == name)
									{
										found = true;
										break;
									}
								}

								if (!found)
									possible_lenses.push_back(name);
							}
						}

						if (!possible_lenses.empty())
						{
							auto it = begin(possible_lenses);
							this->LensInfo.FromMakerNote = *(it++);
							for (; it != end(possible_lenses); ++it)
							{
								this->LensInfo.FromMakerNote.append(" or ").append(*it);
							}
						}
					}
				}
				else
				{
					// nikon format 2
				}
			}
			else
			{
				// nikon format 1
			}
		}
	}
end_of_maker_note:

	// Jump to the GPS SubIFD if it exists and parse all the information
	// there. Note that it's possible that the GPS SubIFD doesn't exist.
	if (gps_sub_ifd_offset + 4 <= len)
	{
		offs = gps_sub_ifd_offset;
		int num_entries = parse_value<uint16_t>(buf + offs, alignIntel);
		if (offs + 6 + 12 * num_entries > len)
			return PARSE_EXIF_ERROR_CORRUPT;
		offs += 2;
		while (--num_entries >= 0)
		{
			unsigned short tag, format;
			unsigned length, data;
			parseIFEntryHeader(buf + offs, alignIntel, tag, format, length, data);
			switch (tag)
			{
			case 1:
				// GPS north or south
				this->GeoLocation.LatComponents.direction = *(buf + offs + 8);
				if ('S' == this->GeoLocation.LatComponents.direction)
					this->GeoLocation.Latitude = -this->GeoLocation.Latitude;
				break;

			case 2:
				// GPS latitude
				if (format == 5 && length == 3)
				{
					this->GeoLocation.LatComponents.degrees =
						parse_value<Rational>(buf + data + tiff_header_start, alignIntel);
					this->GeoLocation.LatComponents.minutes =
						parse_value<Rational>(buf + data + tiff_header_start + 8, alignIntel);
					this->GeoLocation.LatComponents.seconds =
						parse_value<Rational>(buf + data + tiff_header_start + 16, alignIntel);
					this->GeoLocation.Latitude =
						this->GeoLocation.LatComponents.degrees +
						this->GeoLocation.LatComponents.minutes / 60 +
						this->GeoLocation.LatComponents.seconds / 3600;
					if ('S' == this->GeoLocation.LatComponents.direction)
						this->GeoLocation.Latitude = -this->GeoLocation.Latitude;
				}
				break;

			case 3:
				// GPS east or west
				this->GeoLocation.LonComponents.direction = *(buf + offs + 8);
				if ('W' == this->GeoLocation.LonComponents.direction)
					this->GeoLocation.Longitude = -this->GeoLocation.Longitude;
				break;

			case 4:
				// GPS longitude
				if (format == 5 && length == 3)
				{
					this->GeoLocation.LonComponents.degrees =
						parse_value<Rational>(buf + data + tiff_header_start, alignIntel);
					this->GeoLocation.LonComponents.minutes =
						parse_value<Rational>(buf + data + tiff_header_start + 8, alignIntel);
					this->GeoLocation.LonComponents.seconds =
						parse_value<Rational>(buf + data + tiff_header_start + 16, alignIntel);
					this->GeoLocation.Longitude =
						this->GeoLocation.LonComponents.degrees +
						this->GeoLocation.LonComponents.minutes / 60 +
						this->GeoLocation.LonComponents.seconds / 3600;
					if ('W' == this->GeoLocation.LonComponents.direction)
						this->GeoLocation.Longitude = -this->GeoLocation.Longitude;
				}
				break;

			case 5:
				// GPS altitude reference (below or above sea level)
				this->GeoLocation.AltitudeRef = *(buf + offs + 8);
				if (1 == this->GeoLocation.AltitudeRef)
					this->GeoLocation.Altitude = -this->GeoLocation.Altitude;
				break;

			case 6:
				// GPS altitude reference
				if (format == 5)
				{
					this->GeoLocation.Altitude =
						parse_value<Rational>(buf + data + tiff_header_start, alignIntel);
					if (1 == this->GeoLocation.AltitudeRef)
						this->GeoLocation.Altitude = -this->GeoLocation.Altitude;
				}
				break;
			}
			offs += 12;
		}
	}

	return PARSE_EXIF_SUCCESS;
}


void EXIFInfo::nikon_decrypt(unsigned char *buf, unsigned len, uint32_t count, uint32_t serial)
{
	static const uint8_t xlat[2][256] = {
		{ 0xc1, 0xbf, 0x6d, 0x0d, 0x59, 0xc5, 0x13, 0x9d, 0x83, 0x61, 0x6b, 0x4f, 0xc7, 0x7f, 0x3d, 0x3d,
		0x53, 0x59, 0xe3, 0xc7, 0xe9, 0x2f, 0x95, 0xa7, 0x95, 0x1f, 0xdf, 0x7f, 0x2b, 0x29, 0xc7, 0x0d,
		0xdf, 0x07, 0xef, 0x71, 0x89, 0x3d, 0x13, 0x3d, 0x3b, 0x13, 0xfb, 0x0d, 0x89, 0xc1, 0x65, 0x1f,
		0xb3, 0x0d, 0x6b, 0x29, 0xe3, 0xfb, 0xef, 0xa3, 0x6b, 0x47, 0x7f, 0x95, 0x35, 0xa7, 0x47, 0x4f,
		0xc7, 0xf1, 0x59, 0x95, 0x35, 0x11, 0x29, 0x61, 0xf1, 0x3d, 0xb3, 0x2b, 0x0d, 0x43, 0x89, 0xc1,
		0x9d, 0x9d, 0x89, 0x65, 0xf1, 0xe9, 0xdf, 0xbf, 0x3d, 0x7f, 0x53, 0x97, 0xe5, 0xe9, 0x95, 0x17,
		0x1d, 0x3d, 0x8b, 0xfb, 0xc7, 0xe3, 0x67, 0xa7, 0x07, 0xf1, 0x71, 0xa7, 0x53, 0xb5, 0x29, 0x89,
		0xe5, 0x2b, 0xa7, 0x17, 0x29, 0xe9, 0x4f, 0xc5, 0x65, 0x6d, 0x6b, 0xef, 0x0d, 0x89, 0x49, 0x2f,
		0xb3, 0x43, 0x53, 0x65, 0x1d, 0x49, 0xa3, 0x13, 0x89, 0x59, 0xef, 0x6b, 0xef, 0x65, 0x1d, 0x0b,
		0x59, 0x13, 0xe3, 0x4f, 0x9d, 0xb3, 0x29, 0x43, 0x2b, 0x07, 0x1d, 0x95, 0x59, 0x59, 0x47, 0xfb,
		0xe5, 0xe9, 0x61, 0x47, 0x2f, 0x35, 0x7f, 0x17, 0x7f, 0xef, 0x7f, 0x95, 0x95, 0x71, 0xd3, 0xa3,
		0x0b, 0x71, 0xa3, 0xad, 0x0b, 0x3b, 0xb5, 0xfb, 0xa3, 0xbf, 0x4f, 0x83, 0x1d, 0xad, 0xe9, 0x2f,
		0x71, 0x65, 0xa3, 0xe5, 0x07, 0x35, 0x3d, 0x0d, 0xb5, 0xe9, 0xe5, 0x47, 0x3b, 0x9d, 0xef, 0x35,
		0xa3, 0xbf, 0xb3, 0xdf, 0x53, 0xd3, 0x97, 0x53, 0x49, 0x71, 0x07, 0x35, 0x61, 0x71, 0x2f, 0x43,
		0x2f, 0x11, 0xdf, 0x17, 0x97, 0xfb, 0x95, 0x3b, 0x7f, 0x6b, 0xd3, 0x25, 0xbf, 0xad, 0xc7, 0xc5,
		0xc5, 0xb5, 0x8b, 0xef, 0x2f, 0xd3, 0x07, 0x6b, 0x25, 0x49, 0x95, 0x25, 0x49, 0x6d, 0x71, 0xc7 },
		{ 0xa7, 0xbc, 0xc9, 0xad, 0x91, 0xdf, 0x85, 0xe5, 0xd4, 0x78, 0xd5, 0x17, 0x46, 0x7c, 0x29, 0x4c,
		0x4d, 0x03, 0xe9, 0x25, 0x68, 0x11, 0x86, 0xb3, 0xbd, 0xf7, 0x6f, 0x61, 0x22, 0xa2, 0x26, 0x34,
		0x2a, 0xbe, 0x1e, 0x46, 0x14, 0x68, 0x9d, 0x44, 0x18, 0xc2, 0x40, 0xf4, 0x7e, 0x5f, 0x1b, 0xad,
		0x0b, 0x94, 0xb6, 0x67, 0xb4, 0x0b, 0xe1, 0xea, 0x95, 0x9c, 0x66, 0xdc, 0xe7, 0x5d, 0x6c, 0x05,
		0xda, 0xd5, 0xdf, 0x7a, 0xef, 0xf6, 0xdb, 0x1f, 0x82, 0x4c, 0xc0, 0x68, 0x47, 0xa1, 0xbd, 0xee,
		0x39, 0x50, 0x56, 0x4a, 0xdd, 0xdf, 0xa5, 0xf8, 0xc6, 0xda, 0xca, 0x90, 0xca, 0x01, 0x42, 0x9d,
		0x8b, 0x0c, 0x73, 0x43, 0x75, 0x05, 0x94, 0xde, 0x24, 0xb3, 0x80, 0x34, 0xe5, 0x2c, 0xdc, 0x9b,
		0x3f, 0xca, 0x33, 0x45, 0xd0, 0xdb, 0x5f, 0xf5, 0x52, 0xc3, 0x21, 0xda, 0xe2, 0x22, 0x72, 0x6b,
		0x3e, 0xd0, 0x5b, 0xa8, 0x87, 0x8c, 0x06, 0x5d, 0x0f, 0xdd, 0x09, 0x19, 0x93, 0xd0, 0xb9, 0xfc,
		0x8b, 0x0f, 0x84, 0x60, 0x33, 0x1c, 0x9b, 0x45, 0xf1, 0xf0, 0xa3, 0x94, 0x3a, 0x12, 0x77, 0x33,
		0x4d, 0x44, 0x78, 0x28, 0x3c, 0x9e, 0xfd, 0x65, 0x57, 0x16, 0x94, 0x6b, 0xfb, 0x59, 0xd0, 0xc8,
		0x22, 0x36, 0xdb, 0xd2, 0x63, 0x98, 0x43, 0xa1, 0x04, 0x87, 0x86, 0xf7, 0xa6, 0x26, 0xbb, 0xd6,
		0x59, 0x4d, 0xbf, 0x6a, 0x2e, 0xaa, 0x2b, 0xef, 0xe6, 0x78, 0xb6, 0x4e, 0xe0, 0x2f, 0xdc, 0x7c,
		0xbe, 0x57, 0x19, 0x32, 0x7e, 0x2a, 0xd0, 0xb8, 0xba, 0x29, 0x00, 0x3c, 0x52, 0x7d, 0xa8, 0x49,
		0x3b, 0x2d, 0xeb, 0x25, 0x49, 0xfa, 0xa3, 0xaa, 0x39, 0xa7, 0xc5, 0xa7, 0x50, 0x11, 0x36, 0xfb,
		0xc6, 0x67, 0x4a, 0xf5, 0xa5, 0x12, 0x65, 0x7e, 0xb0, 0xdf, 0xaf, 0x4e, 0xb3, 0x61, 0x7f, 0x2f }
	};


	uint8_t key = 0;
	for (int i = 0; i < 4; ++i)
	{
		key ^= (count >> (i * 8)) & 0xff;
	}

	uint8_t ci = xlat[0][serial & 0xff];
	uint8_t cj = xlat[1][key];
	uint8_t ck = 0x60;

	for (unsigned i = 0; i < len; ++i)
	{
		cj += ci * ck++;
		buf[i] ^= cj;
	}
}






void EXIFInfo::clear() {
  // Strings
  ImageDescription  = "";
  Make              = "";
  Model             = "";
  Software          = "";
  DateTime          = "";
  DateTimeOriginal  = "";
  DateTimeDigitized = ""; 
  SubSecTimeOriginal= "";
  Copyright         = "";
  Comment           = "";
  SerialNumber      = "";

  // Shorts / unsigned / double
  ByteAlign         = 0;
  Orientation       = 0; 

  BitsPerSample     = 0;
  ExposureTime      = 0;
  FNumber           = 0;
  ISOSpeedRatings   = 0;
  ShutterSpeedValue = 0;
  ExposureBiasValue = 0;
  SubjectDistance   = 0;
  FocalLength       = 0;
  FocalLengthIn35mm = 0;
  Flash             = 0;
  MeteringMode      = 0;
  ImageWidth        = 0;
  ImageHeight       = 0;

  // Geolocation
  GeoLocation.Latitude    = 0;
  GeoLocation.Longitude   = 0;
  GeoLocation.Altitude    = 0;
  GeoLocation.AltitudeRef = 0;
  GeoLocation.LatComponents.degrees   = 0;
  GeoLocation.LatComponents.minutes   = 0;
  GeoLocation.LatComponents.seconds   = 0;
  GeoLocation.LatComponents.direction = 0;
  GeoLocation.LonComponents.degrees   = 0;
  GeoLocation.LonComponents.minutes   = 0;
  GeoLocation.LonComponents.seconds   = 0;
  GeoLocation.LonComponents.direction = 0;

  // LensInfo
  LensInfo.FocalLengthMax = 0;
  LensInfo.FocalLengthMin = 0;
  LensInfo.FStopMax       = 0;
  LensInfo.FStopMin       = 0;
  LensInfo.Make           = "";
  LensInfo.Model          = "";
  LensInfo.SerialNumber = "";
  LensInfo.FromMakerNote  = "";  
}